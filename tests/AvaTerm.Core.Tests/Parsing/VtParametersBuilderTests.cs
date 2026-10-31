using AvaTerm.Core.Parsing;
using Xunit;

namespace AvaTerm.Core.Tests.Parsing;

public sealed class VtParametersBuilderTests
{
    private const int DefaultMaxValueCount = 64;

    public static TheoryData<string, int?[][]> ParameterCases => new()
    {
        { "", [] },
        { "1;23;456", [[1], [23], [456]] },
        { "1:23:456", [[1, 23, 456]] },
        { "1;2:3;4:5", [[1], [2, 3], [4, 5]] },
        { ";", [[null], [null]] },
        { "1;", [[1], [null]] },
        { ";1;;", [[null], [1], [null], [null]] },
        { ":", [[null, null]] },
        { "1:", [[1, null]] },
        { "::1::", [[null, null, 1, null, null]] },
        { ":;", [[null, null], [null]] },
        { ";:", [[null], [null, null]] },
        { "38:2::255:0:0;1", [[38, 2, null, 255, 0, 0], [1]] }
    };

    public static TheoryData<int, string, int> AcceptedNumberCases => new()
    {
        { int.MaxValue, "0", 0 },
        { int.MaxValue, "9", 9 },
        { int.MaxValue, "12345", 12345 },
        { int.MaxValue, "000", 0 },
        { int.MaxValue, "00012", 12 },
        { int.MaxValue, new string('0', 20) + "1", 1 },
        { 0, "0", 0 },
        { 0, "000", 0 },
        { 5, "5", 5 },
        { 5, "05", 5 },
        { 125, "119", 119 },
        { 125, "124", 124 },
        { 125, "125", 125 },
        { int.MaxValue, "2147483647", int.MaxValue }
    };

    public static TheoryData<int, string> RejectedNumberCases => new()
    {
        { 0, "1" },
        { 5, "6" },
        { 5, "06" },
        { 125, "126" },
        { 125, "130" },
        { int.MaxValue, "2147483648" },
        { int.MaxValue, "2147483650" },
        { 5, "1:6" },
        { 5, "1;6" }
    };

    public static TheoryData<int, string, int?[][]> FullCapacityCases => new()
    {
        { 1, "123", [[123]] },
        { 2, ":", [[null, null]] },
        { 2, ";", [[null], [null]] },
        { 3, "1;2;3", [[1], [2], [3]] },
        { 3, "1:2:3", [[1, 2, 3]] },
        { 3, "1;2:3", [[1], [2, 3]] },
        { 3, ";;", [[null], [null], [null]] },
        { 3, "::", [[null, null, null]] },
        { 3, ":;", [[null, null], [null]] }
    };

    public static TheoryData<int, string, byte> ExcessCapacityCases
    {
        get
        {
            var cases = new TheoryData<int, string, byte>();
            (int Capacity, string Prefix)[] prefixes =
            [
                (1, ""),
                (1, "123"),
                (3, "1;2;3"),
                (3, "1:2:3"),
                (3, "1;2:3"),
                (3, ";;"),
                (3, "::"),
                (3, ":;")
            ];

            foreach (var (capacity, prefix) in prefixes)
            {
                foreach (var separator in ":;")
                {
                    cases.Add(capacity, prefix, (byte)separator);
                }
            }

            return cases;
        }
    }

    public static TheoryData<string, int?[][]> PendingValueCases => new()
    {
        { "1;2;", [[1], [2], [3]] },
        { "1:2:", [[1, 2, 3]] }
    };

    public static TheoryData<string, int?[][], string, int?[][]> BuildContinuationCases => new()
    {
        { "", [], "2", [[2]] },
        { "12", [[12]], "3", [[123]] },
        { "1:", [[1, null]], "2", [[1, 2]] },
        { "1;", [[1], [null]], "2", [[1], [2]] }
    };

    [Theory]
    [MemberData(nameof(ParameterCases))]
    public void TryAppend_ParameterText_BuildsExpectedGroups(string input, int?[][] expectedGroups)
    {
        var builder = new VtParametersBuilder(int.MaxValue, DefaultMaxValueCount);

        AppendSuccessfully(builder, input);

        AssertGroups(builder.Build(), expectedGroups);
    }

    [Theory]
    [MemberData(nameof(AcceptedNumberCases))]
    public void TryAppend_NumberWithinLimit_BuildsExpectedValue(int maxValue, string input, int expectedValue)
    {
        var builder = new VtParametersBuilder(maxValue, 1);

        AppendSuccessfully(builder, input);

        AssertGroups(builder.Build(), [expectedValue]);
    }

    [Theory]
    [MemberData(nameof(RejectedNumberCases))]
    public void TryAppend_NumberExceedingLimit_RejectsFirstExcessDigit(int maxValue, string input)
    {
        var builder = new VtParametersBuilder(maxValue, DefaultMaxValueCount);

        AssertLastAppendFails(builder, input);
    }

    [Fact]
    public void TryAppend_SeparateValues_AppliesNumericLimitIndependently()
    {
        var builder = new VtParametersBuilder(5, 3);

        AppendSuccessfully(builder, "1:5;5");

        AssertGroups(builder.Build(), [1, 5], [5]);
    }

    [Theory]
    [MemberData(nameof(FullCapacityCases))]
    public void TryAppend_ExactlyFullCapacity_BuildsAllValues(int capacity, string input, int?[][] expectedGroups)
    {
        var builder = new VtParametersBuilder(int.MaxValue, capacity);

        AppendSuccessfully(builder, input);

        AssertGroups(builder.Build(), expectedGroups);
    }

    [Theory]
    [MemberData(nameof(ExcessCapacityCases))]
    public void TryAppend_AddingValueBeyondCapacity_RejectsSeparator(int capacity, string prefix, byte separator)
    {
        var builder = new VtParametersBuilder(int.MaxValue, capacity);
        AppendSuccessfully(builder, prefix);

        Assert.False(builder.TryAppend(separator));
    }

    [Theory]
    [MemberData(nameof(PendingValueCases))]
    public void TryAppend_FullCapacity_AllowsFillingPendingValue(string prefix, int?[][] expectedGroups)
    {
        var builder = new VtParametersBuilder(int.MaxValue, 3);
        AppendSuccessfully(builder, prefix);

        Assert.True(builder.TryAppend((byte)'3'));

        AssertGroups(builder.Build(), expectedGroups);
    }

    [Theory]
    [InlineData(':')]
    [InlineData(';')]
    public void TryAppend_LimitOf64_Accepts64ValuesAndRejects65th(char separator)
    {
        var builder = new VtParametersBuilder(int.MaxValue, DefaultMaxValueCount);
        var input = string.Join(separator.ToString(), Enumerable.Repeat("1", DefaultMaxValueCount));
        int?[][] expectedGroups = separator == ':'
            ? [Enumerable.Repeat<int?>(1, DefaultMaxValueCount).ToArray()]
            : Enumerable.Range(0, DefaultMaxValueCount).Select(_ => new int?[] { 1 }).ToArray();

        AppendSuccessfully(builder, input);
        AssertGroups(builder.Build(), expectedGroups);

        Assert.False(builder.TryAppend((byte)separator));
    }

    [Theory]
    [InlineData("126")]
    [InlineData("1:2:3:")]
    [InlineData("1;2;3;")]
    public void TryAppend_AfterFailure_RejectsFurtherInput(string input)
    {
        var builder = new VtParametersBuilder(125, 3);
        AssertLastAppendFails(builder, input);

        // Without the failure state, appending '0' would still fit the current numeric limit.
        foreach (var next in "0:;1")
        {
            Assert.False(builder.TryAppend((byte)next));
        }
    }

    [Theory]
    [InlineData("126")]
    [InlineData("1:2:3:")]
    [InlineData("1;2;3;")]
    public void Reset_AfterFailure_ClearsFailureAndPreviousParameters(string input)
    {
        var builder = new VtParametersBuilder(125, 3);
        AssertLastAppendFails(builder, input);

        builder.Reset();

        AssertGroups(builder.Build());
        AppendSuccessfully(builder, ":;");
        AssertGroups(builder.Build(), [null, null], [null]);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Reset_ExistingParameters_ClearsStateBeforeOrAfterBuild(bool buildBeforeReset)
    {
        var builder = new VtParametersBuilder(int.MaxValue, DefaultMaxValueCount);
        AppendSuccessfully(builder, "12:34;56");
        if (buildBeforeReset)
        {
            AssertGroups(builder.Build(), [12, 34], [56]);
        }

        builder.Reset();

        AssertGroups(builder.Build());
        AppendSuccessfully(builder, "7:8;9");
        AssertGroups(builder.Build(), [7, 8], [9]);
    }

    [Fact]
    public void Reset_ReusedBuffers_DoNotLeakOldValuesOrGroups()
    {
        var builder = new VtParametersBuilder(125, 3);
        AppendSuccessfully(builder, "12:34;56");
        AssertGroups(builder.Build(), [12, 34], [56]);

        builder.Reset();
        AppendSuccessfully(builder, ";:");
        AssertGroups(builder.Build(), [null], [null, null]);

        builder.Reset();
        AppendSuccessfully(builder, "7");
        AssertGroups(builder.Build(), [7]);
    }

    [Fact]
    public void Reset_EmptyOrAlreadyReset_RemainsReusable()
    {
        var builder = new VtParametersBuilder(int.MaxValue, DefaultMaxValueCount);

        builder.Reset();
        AssertGroups(builder.Build());
        builder.Reset();
        AssertGroups(builder.Build());

        AppendSuccessfully(builder, "1;2:3");
        AssertGroups(builder.Build(), [1], [2, 3]);
    }

    [Theory]
    [MemberData(nameof(BuildContinuationCases))]
    public void Build_RepeatedCalls_DoNotConsumeOrFinalizeParameters(
        string prefix, int?[][] prefixGroups, string remaining, int?[][] expectedGroups)
    {
        var builder = new VtParametersBuilder(int.MaxValue, DefaultMaxValueCount);
        AppendSuccessfully(builder, prefix);

        AssertGroups(builder.Build(), prefixGroups);
        AssertGroups(builder.Build(), prefixGroups);

        AppendSuccessfully(builder, remaining);
        AssertGroups(builder.Build(), expectedGroups);
    }

    [Fact]
    public void TryAppendBuildAndReset_AfterWarmup_DoNotAllocate()
    {
        const int iterations = 1000;
        const int expectedValueSum = 296; // 38 + 2 + 255 + 0 + 0 + 1
        var builder = new VtParametersBuilder(int.MaxValue, DefaultMaxValueCount);
        var input = "38:2::255:0:0;1"u8.ToArray();

        var warmupSum = 0;
        for (var i = 0; i < iterations; i++)
        {
            warmupSum += BuildAndSumValues(builder, input);
        }

        Assert.Equal(iterations * expectedValueSum, warmupSum);

        // Keep assertions and data preparation outside the measured region.
        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();
        var actualSum = 0;
        for (var i = 0; i < iterations; i++)
        {
            actualSum += BuildAndSumValues(builder, input);
        }

        var allocatedBytes = GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;

        Assert.Equal(iterations * expectedValueSum, actualSum);
        Assert.Equal(0L, allocatedBytes);
    }

    private static void AppendSuccessfully(VtParametersBuilder builder, string input)
    {
        for (var index = 0; index < input.Length; index++)
        {
            Assert.True(
                builder.TryAppend((byte)input[index]),
                $"Input '{input}' was rejected at index {index} ('{input[index]}').");
        }
    }

    private static void AssertLastAppendFails(VtParametersBuilder builder, string input)
    {
        AppendSuccessfully(builder, input[..^1]);
        Assert.False(builder.TryAppend((byte)input[^1]));
    }

    private static void AssertGroups(VtParameters parameters, params int?[][] expectedGroups)
    {
        Assert.Equal(expectedGroups.Length, parameters.Length);
        for (var index = 0; index < expectedGroups.Length; index++)
        {
            Assert.Equal(expectedGroups[index], parameters[index].ToArray());
        }
    }

    private static int BuildAndSumValues(VtParametersBuilder builder, ReadOnlySpan<byte> input)
    {
        builder.Reset();
        foreach (var value in input)
        {
            if (!builder.TryAppend(value))
            {
                return -1;
            }
        }

        var sum = 0;
        foreach (var group in builder.Build())
        {
            foreach (var value in group)
            {
                sum += value.GetValueOrDefault();
            }
        }

        return sum;
    }
}