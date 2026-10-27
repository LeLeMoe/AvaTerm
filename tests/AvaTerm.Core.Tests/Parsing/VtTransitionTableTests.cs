using AvaTerm.Core.Parsing;
using Xunit;

namespace AvaTerm.Core.Tests.Parsing;

public sealed class VtTransitionTableTests
{
    [Fact]
    public void Get_AllStateAndByteCombinations_HaveExpectedValidity()
    {
        foreach (var state in Enum.GetValues<VtState>())
        {
            // Use int so that advancing past 0xFF cannot wrap back to zero.
            for (int input = byte.MinValue; input <= byte.MaxValue; ++input)
            {
                var transition = VtTransitionTable.Get(state, (byte)input);
                var context = $"State {state}, input 0x{input:X2}, actual {transition}.";

                if (state == VtState.Invalid)
                {
                    Assert.True(
                        transition.NextState == VtState.Invalid && transition.Action == VtAction.Invalid,
                        $"{context} The Invalid row must return both Invalid state and Invalid action.");
                }
                else
                {
                    Assert.True(
                        transition.NextState != VtState.Invalid && transition.Action != VtAction.Invalid,
                        $"{context} A valid row must return neither Invalid state nor Invalid action.");
                }
            }
        }
    }

    [Theory]
    [InlineData(0x18)] // CAN
    [InlineData(0x1A)] // SUB
    public void Get_CancelControl_CancelsEveryValidState(byte input)
    {
        foreach (var state in Enum.GetValues<VtState>())
        {
            if (state != VtState.Invalid)
            {
                AssertTransitions((state, input, VtState.Ground, VtAction.CancelSequence));
            }
        }
    }

    [Fact]
    public void Get_Escape_UsesStateSpecificTransition()
    {
        VtState[] commandStates =
        [
            VtState.Ground, VtState.Escape, VtState.EscapeIntermediate, VtState.EscapeIgnore,
            VtState.CsiEntry, VtState.CsiIntermediate, VtState.CsiParam, VtState.CsiIgnore,
            VtState.DcsEntry, VtState.DcsIntermediate, VtState.DcsParam
        ];

        foreach (var state in commandStates)
        {
            AssertTransitions((state, 0x1B, VtState.Escape, VtAction.Clear));
        }

        VtState[] stringStates =
        [
            VtState.DcsIgnore, VtState.DcsPassthrough, VtState.OscString,
            VtState.ApcString, VtState.IgnoredString
        ];

        foreach (var state in stringStates)
        {
            AssertTransitions((state, 0x1B, VtState.StringEscape, VtAction.Ignore));
        }

        AssertTransitions((VtState.StringEscape, 0x1B, VtState.Escape, VtAction.InterruptSequence));
    }

    [Fact]
    public void Get_StringTerminators_SelectEndPutOrIgnore()
    {
        AssertTransitions(
            (VtState.OscString, 0x07, VtState.Ground, VtAction.StringEnd),
            (VtState.ApcString, 0x07, VtState.ApcString, VtAction.Ignore),
            (VtState.DcsPassthrough, 0x07, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.DcsIgnore, 0x07, VtState.DcsIgnore, VtAction.Ignore),
            (VtState.IgnoredString, 0x07, VtState.IgnoredString, VtAction.Ignore),
            (VtState.StringEscape, 0x5C, VtState.Ground, VtAction.StringEnd),
            (VtState.Escape, 0x5C, VtState.Ground, VtAction.Ignore),
            (VtState.EscapeIntermediate, 0x5C, VtState.Ground, VtAction.EscDispatch));
    }

    [Theory]
    [InlineData(0x00)]
    [InlineData(0x17)]
    [InlineData(0x19)]
    [InlineData(0x1C)]
    [InlineData(0x5B)]
    [InlineData(0x5D)]
    [InlineData(0x7F)]
    [InlineData(0xFF)]
    public void Get_StringEscapeNonTerminator_SelectsInterrupt(byte input)
    {
        AssertTransitions((VtState.StringEscape, input, VtState.Escape, VtAction.InterruptSequence));
    }

    [Fact]
    public void Get_GroundBoundaries_DistinguishControlsTextAndDel()
    {
        AssertTransitions(
            (VtState.Ground, 0x00, VtState.Ground, VtAction.Execute),
            (VtState.Ground, 0x1F, VtState.Ground, VtAction.Execute),
            (VtState.Ground, 0x20, VtState.Ground, VtAction.Print),
            (VtState.Ground, 0x7E, VtState.Ground, VtAction.Print),
            (VtState.Ground, 0x7F, VtState.Ground, VtAction.Ignore),
            (VtState.Ground, 0x80, VtState.Ground, VtAction.Print),
            (VtState.Ground, 0x9B, VtState.Ground, VtAction.Print),
            (VtState.Ground, 0x9C, VtState.Ground, VtAction.Print),
            (VtState.Ground, 0xFF, VtState.Ground, VtAction.Print));
    }

    [Fact]
    public void Get_EscapeBoundaries_DistinguishIntermediatesFinalsAndInvalidBytes()
    {
        AssertTransitions(
            (VtState.Escape, 0x2F, VtState.EscapeIntermediate, VtAction.Collect),
            (VtState.Escape, 0x30, VtState.Ground, VtAction.EscDispatch),
            (VtState.Escape, 0x7E, VtState.Ground, VtAction.EscDispatch),
            (VtState.Escape, 0x7F, VtState.Escape, VtAction.Ignore),
            (VtState.Escape, 0x80, VtState.EscapeIgnore, VtAction.Ignore),
            (VtState.EscapeIntermediate, 0x2F, VtState.EscapeIntermediate, VtAction.Collect),
            (VtState.EscapeIntermediate, 0x30, VtState.Ground, VtAction.EscDispatch),
            (VtState.EscapeIntermediate, 0x7F, VtState.EscapeIntermediate, VtAction.Ignore),
            (VtState.EscapeIntermediate, 0x80, VtState.EscapeIgnore, VtAction.Ignore),
            (VtState.EscapeIgnore, 0x2F, VtState.EscapeIgnore, VtAction.Ignore),
            (VtState.EscapeIgnore, 0x30, VtState.Ground, VtAction.Ignore),
            (VtState.EscapeIgnore, 0x7E, VtState.Ground, VtAction.Ignore),
            (VtState.EscapeIgnore, 0x7F, VtState.EscapeIgnore, VtAction.Ignore),
            (VtState.EscapeIgnore, 0x80, VtState.EscapeIgnore, VtAction.Ignore));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Get_CommandHeaderBoundaries_SelectExpectedTransitions(bool isDcs)
    {
        var entry = isDcs ? VtState.DcsEntry : VtState.CsiEntry;
        var param = isDcs ? VtState.DcsParam : VtState.CsiParam;
        var intermediate = isDcs ? VtState.DcsIntermediate : VtState.CsiIntermediate;
        var ignore = isDcs ? VtState.DcsIgnore : VtState.CsiIgnore;
        var finalState = isDcs ? VtState.DcsPassthrough : VtState.Ground;
        var finalAction = isDcs ? VtAction.StringStart : VtAction.CsiDispatch;
        var controlAction = isDcs ? VtAction.Ignore : VtAction.Execute;
        var ignoredFinalState = isDcs ? VtState.DcsIgnore : VtState.Ground;

        AssertTransitions(
            (entry, 0x1F, entry, controlAction),
            (entry, 0x20, intermediate, VtAction.Collect),
            (entry, 0x2F, intermediate, VtAction.Collect),
            (entry, 0x30, param, VtAction.Param),
            (entry, 0x3A, param, VtAction.Param), // Colon subparameters are accepted.
            (entry, 0x3B, param, VtAction.Param),
            (entry, 0x3C, param, VtAction.Collect),
            (entry, 0x3F, param, VtAction.Collect),
            (entry, 0x40, finalState, finalAction),
            (entry, 0x7E, finalState, finalAction),
            (entry, 0x7F, entry, VtAction.Ignore),
            (entry, 0x80, ignore, VtAction.Ignore),
            (entry, 0xFF, ignore, VtAction.Ignore),
            (param, 0x2F, intermediate, VtAction.Collect),
            (param, 0x30, param, VtAction.Param),
            (param, 0x3A, param, VtAction.Param),
            (param, 0x3B, param, VtAction.Param),
            (param, 0x3C, ignore, VtAction.Ignore), // Private prefixes are only accepted in Entry.
            (param, 0x3F, ignore, VtAction.Ignore),
            (param, 0x40, finalState, finalAction),
            (param, 0x7F, param, VtAction.Ignore),
            (param, 0x80, ignore, VtAction.Ignore),
            (intermediate, 0x2F, intermediate, VtAction.Collect),
            (intermediate, 0x30, ignore, VtAction.Ignore),
            (intermediate, 0x3F, ignore, VtAction.Ignore),
            (intermediate, 0x40, finalState, finalAction),
            (intermediate, 0x7F, intermediate, VtAction.Ignore),
            (intermediate, 0x80, ignore, VtAction.Ignore),
            (ignore, 0x3F, ignore, VtAction.Ignore),
            (ignore, 0x40, ignoredFinalState, VtAction.Ignore),
            (ignore, 0x7E, ignoredFinalState, VtAction.Ignore),
            (ignore, 0x80, ignore, VtAction.Ignore));
    }

    [Fact]
    public void Get_StringPayloadBoundaries_ApplyStateSpecificFilters()
    {
        AssertTransitions(
            (VtState.DcsPassthrough, 0x00, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.DcsPassthrough, 0x1F, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.DcsPassthrough, 0x7E, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.DcsPassthrough, 0x7F, VtState.DcsPassthrough, VtAction.Ignore),
            (VtState.DcsPassthrough, 0x80, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.DcsPassthrough, 0x9C, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.DcsPassthrough, 0xFF, VtState.DcsPassthrough, VtAction.StringPut),
            (VtState.OscString, 0x1F, VtState.OscString, VtAction.Ignore),
            (VtState.OscString, 0x20, VtState.OscString, VtAction.StringPut),
            (VtState.OscString, 0x7F, VtState.OscString, VtAction.StringPut),
            (VtState.OscString, 0x9C, VtState.OscString, VtAction.StringPut),
            (VtState.OscString, 0xFF, VtState.OscString, VtAction.StringPut),
            (VtState.ApcString, 0x08, VtState.ApcString, VtAction.StringPut),
            (VtState.ApcString, 0x0D, VtState.ApcString, VtAction.StringPut),
            (VtState.ApcString, 0x0E, VtState.ApcString, VtAction.Ignore),
            (VtState.ApcString, 0x1F, VtState.ApcString, VtAction.Ignore),
            (VtState.ApcString, 0x20, VtState.ApcString, VtAction.StringPut),
            (VtState.ApcString, 0x7E, VtState.ApcString, VtAction.StringPut),
            (VtState.ApcString, 0x7F, VtState.ApcString, VtAction.Ignore),
            (VtState.ApcString, 0x80, VtState.ApcString, VtAction.StringPut),
            (VtState.ApcString, 0x9C, VtState.ApcString, VtAction.StringPut),
            (VtState.ApcString, 0xFF, VtState.ApcString, VtAction.StringPut));
    }

    private static void AssertTransitions(
        params (VtState State, byte Input, VtState NextState, VtAction Action)[] cases)
    {
        foreach (var testCase in cases)
        {
            var expected = new VtTransition(testCase.NextState, testCase.Action);
            var actual = VtTransitionTable.Get(testCase.State, testCase.Input);

            Assert.True(actual == expected,
                $"State {testCase.State}, input 0x{testCase.Input:X2}: expected {expected}, actual {actual}.");
        }
    }
}