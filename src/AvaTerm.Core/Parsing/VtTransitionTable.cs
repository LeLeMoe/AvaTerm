using System.Diagnostics;

namespace AvaTerm.Core.Parsing;

internal enum VtState : byte
{
    Invalid = 0,
    Ground = 1,
    Escape,
    EscapeIntermediate,
    EscapeIgnore,
    CsiEntry,
    CsiIntermediate,
    CsiParam,
    CsiIgnore,
    DcsEntry,
    DcsIntermediate,
    DcsParam,
    DcsIgnore,
    DcsPassthrough,
    OscString,
    ApcString,
    IgnoredString,
    StringEscape
}

internal enum VtAction : byte
{
    Invalid = 0,
    Ignore = 1,
    Print,
    Execute,
    Clear,
    EscDispatch,
    Collect,
    Param,
    CsiDispatch,
    StringStart,
    StringPut,
    StringEnd,
    InterruptSequence,
    CancelSequence
}

internal readonly record struct VtTransition(VtState NextState, VtAction Action);

internal static class VtTransitionTable
{
    private readonly record struct ByteRange(byte Start, byte End)
    {
        public static implicit operator ByteRange(byte value) => new(value, value);
    }

    private static readonly VtTransition[,] Table = CreateTable();

    private static ByteRange Range(byte start, byte end) => new(start, end);

    private static void AddTransition(VtTransition[,] table, VtState state, VtState nextState, VtAction action,
        params ByteRange[] bytes)
    {
        foreach (var range in bytes)
        {
            Debug.Assert(range.Start <= range.End);

            for (int i = range.Start; i <= range.End; ++i)
            {
                Debug.Assert(table[(int)state, i] == new VtTransition(VtState.Invalid, VtAction.Invalid));

                table[(int)state, i] = new VtTransition(nextState, action);
            }
        }
    }

    private static VtTransition[,] CreateTable()
    {
        const int inputCount = byte.MaxValue - byte.MinValue + 1;
        var states = Enum.GetValues<VtState>();
        var table = new VtTransition[states.Length, inputCount];

        // Ground internal transitions
        AddTransition(table, VtState.Ground, VtState.Ground, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.Ground, VtState.Ground, VtAction.Print, Range(0x20, 0x7E), Range(0x80, 0xFF));
        AddTransition(table, VtState.Ground, VtState.Ground, VtAction.Ignore, 0x7F);

        // Escape internal transitions
        AddTransition(table, VtState.Escape, VtState.Escape, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.Escape, VtState.Escape, VtAction.Ignore, 0x7F);

        // From Escape transitions
        AddTransition(table, VtState.Escape, VtState.Ground, VtAction.EscDispatch,
            Range(0x30, 0x4F), Range(0x51, 0x57), Range(0x59, 0x5A), Range(0x60, 0x7E));
        AddTransition(table, VtState.Escape, VtState.Ground, VtAction.Ignore, 0x5C);
        AddTransition(table, VtState.Escape, VtState.EscapeIntermediate, VtAction.Collect, Range(0x20, 0x2F));
        AddTransition(table, VtState.Escape, VtState.EscapeIgnore, VtAction.Ignore, Range(0x80, 0xFF));
        AddTransition(table, VtState.Escape, VtState.CsiEntry, VtAction.Clear, 0x5B);
        AddTransition(table, VtState.Escape, VtState.DcsEntry, VtAction.Clear, 0x50);
        AddTransition(table, VtState.Escape, VtState.OscString, VtAction.StringStart, 0x5D);
        AddTransition(table, VtState.Escape, VtState.ApcString, VtAction.StringStart, 0x5F);
        AddTransition(table, VtState.Escape, VtState.IgnoredString, VtAction.Ignore, 0x58, 0x5E);

        // EscapeIntermediate internal transitions
        AddTransition(table, VtState.EscapeIntermediate, VtState.EscapeIntermediate, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.EscapeIntermediate, VtState.EscapeIntermediate, VtAction.Collect,
            Range(0x20, 0x2F));
        AddTransition(table, VtState.EscapeIntermediate, VtState.EscapeIntermediate, VtAction.Ignore, 0x7F);

        // From EscapeIntermediate transitions
        AddTransition(table, VtState.EscapeIntermediate, VtState.Ground, VtAction.EscDispatch, Range(0x30, 0x7E));
        AddTransition(table, VtState.EscapeIntermediate, VtState.EscapeIgnore, VtAction.Ignore, Range(0x80, 0xFF));

        // EscapeIgnore internal transitions
        AddTransition(table, VtState.EscapeIgnore, VtState.EscapeIgnore, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.EscapeIgnore, VtState.EscapeIgnore, VtAction.Ignore,
            Range(0x20, 0x2F), Range(0x7F, 0xFF));

        // From EscapeIgnore transitions
        AddTransition(table, VtState.EscapeIgnore, VtState.Ground, VtAction.Ignore, Range(0x30, 0x7E));

        // CsiEntry internal transitions
        AddTransition(table, VtState.CsiEntry, VtState.CsiEntry, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.CsiEntry, VtState.CsiEntry, VtAction.Ignore, 0x7F);

        // From CsiEntry transitions
        AddTransition(table, VtState.CsiEntry, VtState.Ground, VtAction.CsiDispatch, Range(0x40, 0x7E));
        AddTransition(table, VtState.CsiEntry, VtState.CsiIntermediate, VtAction.Collect, Range(0x20, 0x2F));
        AddTransition(table, VtState.CsiEntry, VtState.CsiParam, VtAction.Param, Range(0x30, 0x3B));
        AddTransition(table, VtState.CsiEntry, VtState.CsiParam, VtAction.Collect, Range(0x3C, 0x3F));
        AddTransition(table, VtState.CsiEntry, VtState.CsiIgnore, VtAction.Ignore, Range(0x80, 0xFF));

        // CsiIntermediate internal transitions
        AddTransition(table, VtState.CsiIntermediate, VtState.CsiIntermediate, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.CsiIntermediate, VtState.CsiIntermediate, VtAction.Collect, Range(0x20, 0x2F));
        AddTransition(table, VtState.CsiIntermediate, VtState.CsiIntermediate, VtAction.Ignore, 0x7F);

        // From CsiIntermediate transitions
        AddTransition(table, VtState.CsiIntermediate, VtState.Ground, VtAction.CsiDispatch, Range(0x40, 0x7E));
        AddTransition(table, VtState.CsiIntermediate, VtState.CsiIgnore, VtAction.Ignore,
            Range(0x30, 0x3F), Range(0x80, 0xFF));

        // CsiParam internal transitions
        AddTransition(table, VtState.CsiParam, VtState.CsiParam, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.CsiParam, VtState.CsiParam, VtAction.Param, Range(0x30, 0x3B));
        AddTransition(table, VtState.CsiParam, VtState.CsiParam, VtAction.Ignore, 0x7F);

        // From CsiParam transitions
        AddTransition(table, VtState.CsiParam, VtState.Ground, VtAction.CsiDispatch, Range(0x40, 0x7E));
        AddTransition(table, VtState.CsiParam, VtState.CsiIntermediate, VtAction.Collect, Range(0x20, 0x2F));
        AddTransition(table, VtState.CsiParam, VtState.CsiIgnore, VtAction.Ignore,
            Range(0x3C, 0x3F), Range(0x80, 0xFF));

        // CsiIgnore internal transitions
        AddTransition(table, VtState.CsiIgnore, VtState.CsiIgnore, VtAction.Execute,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.CsiIgnore, VtState.CsiIgnore, VtAction.Ignore,
            Range(0x20, 0x3F), Range(0x7F, 0xFF));

        // From CsiIgnore transitions
        AddTransition(table, VtState.CsiIgnore, VtState.Ground, VtAction.Ignore, Range(0x40, 0x7E));

        // DcsEntry internal transitions
        AddTransition(table, VtState.DcsEntry, VtState.DcsEntry, VtAction.Ignore,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F), 0x7F);

        // From DcsEntry transitions
        AddTransition(table, VtState.DcsEntry, VtState.DcsIntermediate, VtAction.Collect, Range(0x20, 0x2F));
        AddTransition(table, VtState.DcsEntry, VtState.DcsParam, VtAction.Param, Range(0x30, 0x3B));
        AddTransition(table, VtState.DcsEntry, VtState.DcsParam, VtAction.Collect, Range(0x3C, 0x3F));
        AddTransition(table, VtState.DcsEntry, VtState.DcsIgnore, VtAction.Ignore, Range(0x80, 0xFF));
        AddTransition(table, VtState.DcsEntry, VtState.DcsPassthrough, VtAction.StringStart, Range(0x40, 0x7E));

        // DcsIntermediate internal transitions
        AddTransition(table, VtState.DcsIntermediate, VtState.DcsIntermediate, VtAction.Ignore,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F), 0x7F);
        AddTransition(table, VtState.DcsIntermediate, VtState.DcsIntermediate, VtAction.Collect, Range(0x20, 0x2F));

        // From DcsIntermediate transitions
        AddTransition(table, VtState.DcsIntermediate, VtState.DcsIgnore, VtAction.Ignore,
            Range(0x30, 0x3F), Range(0x80, 0xFF));
        AddTransition(table, VtState.DcsIntermediate, VtState.DcsPassthrough, VtAction.StringStart, Range(0x40, 0x7E));

        // DcsParam internal transitions
        AddTransition(table, VtState.DcsParam, VtState.DcsParam, VtAction.Ignore,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x1F), 0x7F);
        AddTransition(table, VtState.DcsParam, VtState.DcsParam, VtAction.Param, Range(0x30, 0x3B));

        // From DcsParam transitions
        AddTransition(table, VtState.DcsParam, VtState.DcsIntermediate, VtAction.Collect, Range(0x20, 0x2F));
        AddTransition(table, VtState.DcsParam, VtState.DcsIgnore, VtAction.Ignore,
            Range(0x3C, 0x3F), Range(0x80, 0xFF));
        AddTransition(table, VtState.DcsParam, VtState.DcsPassthrough, VtAction.StringStart, Range(0x40, 0x7E));

        // DcsIgnore internal transitions
        AddTransition(table, VtState.DcsIgnore, VtState.DcsIgnore, VtAction.Ignore,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0xFF));

        // From DcsIgnore transitions
        AddTransition(table, VtState.DcsIgnore, VtState.StringEscape, VtAction.Ignore, 0x1B);

        // DcsPassthrough internal transitions
        AddTransition(table, VtState.DcsPassthrough, VtState.DcsPassthrough, VtAction.StringPut,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0x7E), Range(0x80, 0xFF));
        AddTransition(table, VtState.DcsPassthrough, VtState.DcsPassthrough, VtAction.Ignore, 0x7F);

        // From DcsPassthrough transitions
        AddTransition(table, VtState.DcsPassthrough, VtState.StringEscape, VtAction.Ignore, 0x1B);

        // OscString internal transitions
        AddTransition(table, VtState.OscString, VtState.OscString, VtAction.Ignore,
            Range(0x00, 0x06), Range(0x08, 0x17), 0x19, Range(0x1C, 0x1F));
        AddTransition(table, VtState.OscString, VtState.OscString, VtAction.StringPut, Range(0x20, 0xFF));

        // From OscString transitions
        AddTransition(table, VtState.OscString, VtState.Ground, VtAction.StringEnd, 0x07);
        AddTransition(table, VtState.OscString, VtState.StringEscape, VtAction.Ignore, 0x1B);

        // ApcString internal transitions
        AddTransition(table, VtState.ApcString, VtState.ApcString, VtAction.Ignore,
            Range(0x00, 0x07), Range(0x0E, 0x17), 0x19, Range(0x1C, 0x1F), 0x7F);
        AddTransition(table, VtState.ApcString, VtState.ApcString, VtAction.StringPut,
            Range(0x08, 0x0D), Range(0x20, 0x7E), Range(0x80, 0xFF));

        // From ApcString transitions
        AddTransition(table, VtState.ApcString, VtState.StringEscape, VtAction.Ignore, 0x1B);

        // IgnoredString internal transitions
        AddTransition(table, VtState.IgnoredString, VtState.IgnoredString, VtAction.Ignore,
            Range(0x00, 0x17), 0x19, Range(0x1C, 0xFF));

        // From IgnoredString transitions
        AddTransition(table, VtState.IgnoredString, VtState.StringEscape, VtAction.Ignore, 0x1B);

        // From StringEscape transitions
        AddTransition(table, VtState.StringEscape, VtState.Ground, VtAction.CancelSequence, 0x18, 0x1A);
        AddTransition(table, VtState.StringEscape, VtState.Ground, VtAction.StringEnd, 0x5C);
        AddTransition(table, VtState.StringEscape, VtState.Escape, VtAction.InterruptSequence,
            Range(0x00, 0x17), 0x19, Range(0x1B, 0x5B), Range(0x5D, 0xFF));

        foreach (var state in states)
        {
            // Anywhere to Ground transitions
            if (state != VtState.Invalid && state != VtState.StringEscape)
            {
                AddTransition(table, state, VtState.Ground, VtAction.CancelSequence, 0x18, 0x1A);
            }

            // Anywhere to Escape transitions
            if (state != VtState.Invalid && state != VtState.DcsIgnore && state != VtState.DcsPassthrough &&
                state != VtState.OscString && state != VtState.ApcString && state != VtState.IgnoredString &&
                state != VtState.StringEscape)
            {
                AddTransition(table, state, VtState.Escape, VtAction.Clear, 0x1B);
            }
        }

        return table;
    }

    public static VtTransition Get(VtState state, byte input) => Table[(int)state, input];
}