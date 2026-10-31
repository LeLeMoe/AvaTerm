using AvaTerm.Core.Parsing;
using Xunit;

namespace AvaTerm.Core.Tests.Parsing;

public sealed class VtParametersTests
{
    [Fact]
    public void Indexer_MultipleGroups_ReturnsExpectedSlicesAndGroupCount()
    {
        int?[] values = [10, null, 30, 40, 50, 60];
        Range[] groups = [0..2, 2..3, 3..6];
        var parameters = new VtParameters(values, groups);

        Assert.Equal(3, parameters.Length);
        Assert.False(parameters.IsEmpty);
        Assert.Equal([10, null], parameters[0].ToArray());
        Assert.Equal([30], parameters[1].ToArray());
        Assert.Equal([40, 50, 60], parameters[2].ToArray());
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(2)]
    public void Indexer_InvalidGroupIndex_ThrowsIndexOutOfRangeException(int index)
    {
        int?[] values = [10, 20, 30];
        Range[] groups = [0..1, 1..3];

        Assert.Throws<IndexOutOfRangeException>(() =>
        {
            var parameters = new VtParameters(values, groups);
            _ = parameters[index];
        });
    }

    [Fact]
    public void Indexer_ReturnedSpan_ReflectsChangesToBackingValues()
    {
        int?[] values = [10, 20, 30];
        Range[] groups = [1..3];
        var parameters = new VtParameters(values, groups);
        var group = parameters[0];

        values[1] = null;
        values[2] = 40;

        Assert.Equal([null, 40], group.ToArray());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EmptyParameters_HaveZeroLengthAndDoNotEnumerate(bool useDefault)
    {
        var parameters = useDefault
            ? default
            : new VtParameters(ReadOnlySpan<int?>.Empty, ReadOnlySpan<Range>.Empty);

        Assert.Equal(0, parameters.Length);
        Assert.True(parameters.IsEmpty);
        AssertGroups(parameters);

        var enumerator = parameters.GetEnumerator();
        Assert.False(enumerator.MoveNext());
        Assert.False(enumerator.MoveNext());
    }

    [Fact]
    public void MoveNext_SingleNullParameter_YieldsOneGroup()
    {
        int?[] values = [null];
        Range[] groups = [0..1];
        var parameters = new VtParameters(values, groups);
        var enumerator = parameters.GetEnumerator();

        Assert.False(parameters.IsEmpty);
        Assert.True(enumerator.MoveNext());
        Assert.Equal([null], enumerator.Current.ToArray());
        Assert.Equal([null], enumerator.Current.ToArray());
        Assert.False(enumerator.MoveNext());
        Assert.False(enumerator.MoveNext());
    }

    [Fact]
    public void Foreach_MultipleGroups_YieldsEachGroupInOrder()
    {
        int?[] values = [10, null, 30, 40, 50, 60];
        Range[] groups = [0..2, 2..3, 3..6];
        var parameters = new VtParameters(values, groups);

        AssertGroups(parameters, [10, null], [30], [40, 50, 60]);
    }

    [Fact]
    public void Foreach_OmittedParameters_PreservesTheirPositions()
    {
        int?[] values = [null, 10, null, 20, null];
        Range[] groups = [0..1, 1..2, 2..3, 3..4, 4..5];
        var parameters = new VtParameters(values, groups);

        Assert.Equal(5, parameters.Length);
        AssertGroups(parameters, [null], [10], [null], [20], [null]);
    }

    [Fact]
    public void GetEnumerator_TwoEnumerators_AdvanceIndependently()
    {
        int?[] values = [10, 20];
        Range[] groups = [0..1, 1..2];
        var parameters = new VtParameters(values, groups);
        var first = parameters.GetEnumerator();

        Assert.True(first.MoveNext());
        Assert.Equal([10], first.Current.ToArray());

        var second = parameters.GetEnumerator();
        Assert.True(second.MoveNext());
        Assert.Equal([10], second.Current.ToArray());

        Assert.True(first.MoveNext());
        Assert.Equal([20], first.Current.ToArray());
        Assert.Equal([10], second.Current.ToArray());
        Assert.False(first.MoveNext());
        Assert.False(first.MoveNext());

        Assert.True(second.MoveNext());
        Assert.Equal([20], second.Current.ToArray());
        Assert.False(second.MoveNext());
    }

    [Fact]
    public void GetEnumerator_AfterCompletedEnumeration_StartsAtFirstGroup()
    {
        int?[] values = [10, 20, 30];
        Range[] groups = [0..1, 1..3];
        var parameters = new VtParameters(values, groups);

        AssertGroups(parameters, [10], [20, 30]);
        AssertGroups(parameters, [10], [20, 30]);
    }

    private static void AssertGroups(VtParameters parameters, params int?[][] expectedGroups)
    {
        var index = 0;

        foreach (var group in parameters)
        {
            Assert.True(index < expectedGroups.Length, "Enumerator returned more groups than expected.");
            Assert.Equal(expectedGroups[index], group.ToArray());
            index++;
        }

        Assert.Equal(expectedGroups.Length, index);
    }
}