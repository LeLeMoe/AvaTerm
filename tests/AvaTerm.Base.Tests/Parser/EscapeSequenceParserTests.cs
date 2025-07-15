using System.Reflection;
using System.Text;
using AvaTerm.Data;
using AvaTerm.Parser;
using static AvaTerm.Parser.EscapeSequenceParser;

namespace AvaTerm.Base.Tests.Parser;

public class EscapeSequenceParserTests
{
    private class MockHandler
    {
        private class MockDcsHandler(Action<ActionRecord> callback) : IDcsHandler
        {
            public void Hook(char code, string collect, int[] parameters)
            {
                callback(new ActionRecord("dcs_hook", code, collect, parameters));
            }

            public void Put(ReadOnlySpan<char> text)
            {
                callback(new ActionRecord("dcs_put", text.ToString()));
            }

            public void Unhook()
            {
                callback(new ActionRecord("dcs_unhook"));
            }
        }

        private class MockAuxStringHandler(string prefix, Action<ActionRecord> callback) : IAuxStringHandler
        {
            public void Start()
            {
                callback(new ActionRecord(prefix + "_start"));
            }

            public void Put(ReadOnlySpan<char> text)
            {
                callback(new ActionRecord(prefix + "_put", text.ToString()));
            }

            public void End()
            {
                callback(new ActionRecord(prefix + "_end"));
            }
        }

        public List<ActionRecord> Actions { get; } = [];

        private void PrintHandler(ReadOnlySpan<char> text)
        {
            Record(new ActionRecord("print", text.ToString()));
        }

        private void ExecuteHandler(char code)
        {
            Record(new ActionRecord("execute", code));
        }

        private void EscapeHandler(char code, string collect)
        {
            Record(new ActionRecord("escape", code, collect));
        }

        private void CsiHandler(char code, string collect, int[] parameters)
        {
            Record(new ActionRecord("csi", code, collect, parameters));
        }

        private void Record(ActionRecord record)
        {
            Actions.Add(record);
        }

        public MockHandler(EscapeSequenceParser parser)
        {
            parser.PrintHandler = PrintHandler;
            parser.ExecuteHandler = ExecuteHandler;
            parser.EscapeHandler = EscapeHandler;
            parser.CsiHandler = CsiHandler;
            parser.DcsHandler = new MockDcsHandler(Record);
            parser.OscHandler = new MockAuxStringHandler("osc", Record);
            parser.SosHandler = new MockAuxStringHandler("sos", Record);
            parser.PmHandler = new MockAuxStringHandler("pm", Record);
            parser.ApcHandler = new MockAuxStringHandler("apc", Record);
        }
    }

    public readonly struct ActionRecord : IEquatable<ActionRecord>
    {
        private readonly string _action;
        private readonly char? _code;
        private readonly string? _collect;
        private readonly int[]? _param;
        private readonly string? _text;

        public static bool operator ==(ActionRecord left, ActionRecord right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(ActionRecord left, ActionRecord right)
        {
            return !(left == right);
        }

        public ActionRecord(string action)
        {
            _action = action;
        }

        public ActionRecord(string action, char code) : this(action)
        {
            _code = code;
        }

        public ActionRecord(string action, char code, string collect) : this(action, code)
        {
            _collect = collect;
        }

        public ActionRecord(string action, char code, string collect, int[] param) : this(action, code, collect)
        {
            _param = param;
        }

        public ActionRecord(string action, string text) : this(action)
        {
            _text = text;
        }

        public bool Equals(ActionRecord other)
        {
            if (!string.Equals(_action, other._action))
            {
                return false;
            }

            if (_code != other._code)
            {
                return false;
            }

            if (!string.Equals(_collect, other._collect))
            {
                return false;
            }

            if (_param is null || other._param is null)
            {
                if (!(_param is null && other._param is null))
                {
                    return false;
                }
            }
            else if (!_param.SequenceEqual(other._param))
            {
                return false;
            }

            if (!string.Equals(_text, other._text))
            {
                return false;
            }

            return true;
        }

        public override bool Equals(object? obj)
        {
            return obj is ActionRecord other && Equals(other);
        }

        public override int GetHashCode()
        {
            var hash = new HashCode();

            hash.Add(_action);
            hash.Add(_code);
            hash.Add(_collect);
            hash.Add(_text);

            if (_param is not null)
            {
                foreach (var item in _param)
                {
                    hash.Add(item);
                }
            }

            return hash.ToHashCode();
        }

        public override string ToString()
        {
            var stringBuilder = new StringBuilder();

            stringBuilder.Append("(action: \"");
            stringBuilder.Append(_action);
            stringBuilder.Append('\"');

            if (_code is not null)
            {
                stringBuilder.Append(", code: \"");
                stringBuilder.Append(ToHexString(_code!.ToString()));
                stringBuilder.Append('\"');
            }

            if (_collect is not null)
            {
                stringBuilder.Append(", collect: \"");
                stringBuilder.Append(ToHexString(_collect));
                stringBuilder.Append('\"');
            }

            if (_param is not null)
            {
                stringBuilder.Append(", param: [");

                for (var i = 0; i < _param.Length; ++i)
                {
                    stringBuilder.Append(_param[i]);

                    if (i != _param.Length - 1)
                    {
                        stringBuilder.Append(", ");
                    }
                }

                stringBuilder.Append(']');
            }

            if (_text is not null)
            {
                stringBuilder.Append(", text: \"");
                stringBuilder.Append(ToHexString(_text));
                stringBuilder.Append('\"');
            }

            stringBuilder.Append(')');

            return stringBuilder.ToString();
        }
    }

    public readonly struct TransitionTableCompletenessTestData
    {
        private readonly string _stateName = "";

        public byte Code { get; }
        public int State { get; }

        public TransitionTableCompletenessTestData(byte code, int state, string stateName)
        {
            Code = code;
            State = state;
            _stateName = stateName;
        }

        public override string ToString()
        {
            return $"(code: \"\\x{Code:X4}\", state: {_stateName})";
        }
    }

    public readonly struct MapCodeTestData
    {
        public char RawCode { get; }
        public byte ExpectedCode { get; }

        public MapCodeTestData(char rawCode, byte expectedCode)
        {
            RawCode = rawCode;
            ExpectedCode = expectedCode;
        }

        public override string ToString()
        {
            return $"(code: \"\\x{(int)RawCode:X4}\")";
        }
    }

    public readonly struct ParseTestData
    {
        public bool? LegacyMode { get; }
        public string Text { get; } = "";
        public List<ActionRecord> ExpectedActions { get; } = [];

        public ParseTestData()
        {
        }

        public ParseTestData(string text, List<ActionRecord> expectedActions)
        {
            Text = text;
            ExpectedActions = expectedActions;
        }

        public ParseTestData(bool legacyMode, string text, List<ActionRecord> expectedActions)
            : this(text, expectedActions)
        {
            LegacyMode = legacyMode;
        }

        public override string ToString()
        {
            if (LegacyMode is null)
            {
                return $"(text: \"{ToHexString(Text)}\")";
            }
            else
            {
                return $"(legacyMode: {LegacyMode}, text: \"{ToHexString(Text)}\")";
            }
        }
    }

    private static FieldInfo[] GetAllStates()
    {
        var allStates = typeof(EscapeSequenceParser)
            .GetNestedType("State", BindingFlags.NonPublic)
            ?.GetFields(BindingFlags.Public | BindingFlags.Static);

        Assert.NotNull(allStates);

        return allStates;
    }

    private static FieldInfo[] GetAllActions()
    {
        var allActions = typeof(EscapeSequenceParser)
            .GetNestedType("Action", BindingFlags.NonPublic)
            ?.GetFields(BindingFlags.Public | BindingFlags.Static);

        Assert.NotNull(allActions);

        return allActions;
    }

    private static string ToHexString(string text)
    {
        var stringBuilder = new StringBuilder();

        foreach (var code in text)
        {
            stringBuilder.Append($"\\x{(int)code:X4}");
        }

        return stringBuilder.ToString();
    }

    public static TheoryData<TransitionTableCompletenessTestData> GetTransitionTableCompletenessTestData()
    {
        var data = new TheoryData<TransitionTableCompletenessTestData>();

        // Get all valid states
        var validStates = GetAllStates().Where(s => s.Name != "Invalid");

        // Enumerate all states and code range from 0x00 to 0xA0
        foreach (var state in validStates)
        {
            for (byte code = 0x00; code <= 0xA0; ++code)
            {
                var stateValue = state.GetValue(null);
                Assert.NotNull(stateValue);
                data.Add(new TransitionTableCompletenessTestData(code, (int)stateValue, state.Name));
            }
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetTransitionTableCompletenessTestData))]
    public void TransitionTable_CheckCompleteness(TransitionTableCompletenessTestData data)
    {
        var parser = new EscapeSequenceParser();

        // Get invalid state
        var invalidState = GetAllStates().FirstOrDefault(s => s.Name == "Invalid");
        Assert.NotNull(invalidState);

        // Get invalid action
        var invalidAction = GetAllActions().FirstOrDefault(s => s.Name == "Invalid");
        Assert.NotNull(invalidAction);

        // Get transition table instance
        var transitionTable = typeof(EscapeSequenceParser)
            .GetField("_transitionTable", BindingFlags.NonPublic | BindingFlags.Instance)
            ?.GetValue(parser);
        Assert.NotNull(transitionTable);

        // Get transition function
        var transitionFunc = typeof(EscapeSequenceParser)
            .GetNestedType("TransitionTable", BindingFlags.NonPublic)
            ?.GetMethod("Transition", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(transitionFunc);

        // Query the transition table
        var result = transitionFunc.Invoke(transitionTable, [data.Code, data.State]);
        Assert.NotNull(result);

        // Deconstruct the returned tuple
        var resultType = result.GetType();
        var resultActionItem = resultType.GetField("Item1");
        var resultStateItem = resultType.GetField("Item2");
        Assert.NotNull(resultActionItem);
        Assert.NotNull(resultStateItem);

        var resultActionValue = resultActionItem.GetValue(result);
        var resultStateValue = resultStateItem.GetValue(result);
        Assert.NotNull(resultActionValue);
        Assert.NotNull(resultStateValue);

        var resultActionName = Enum.GetName(resultActionItem.FieldType, resultActionValue);
        var resultStateName = Enum.GetName(resultStateItem.FieldType, resultStateValue);
        Assert.NotNull(resultActionName);
        Assert.NotNull(resultStateName);

        // Check results
        Assert.NotEqual(invalidAction.Name, resultActionName);
        Assert.NotEqual(invalidState.Name, resultStateName);
    }

    public static TheoryData<MapCodeTestData> GetMapCodeTestDataForLegacyMode()
    {
        var data = new TheoryData<MapCodeTestData>();

        // Invalid Unicode characters
        data.Add(new MapCodeTestData('\u0100', 0x00));
        data.Add(new MapCodeTestData('\uFFFF', 0x00));
        data.Add(new MapCodeTestData('\u5948', 0x00));

        // All characters in GR area
        for (var code = '\u00A0'; code <= '\u00FF'; ++code)
        {
            data.Add(new MapCodeTestData(code, (byte)(code - 0x0080)));
        }

        // All characters in C0, GL and C1 areas
        for (var code = '\u0000'; code <= '\u009F'; ++code)
        {
            data.Add(new MapCodeTestData(code, (byte)code));
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetMapCodeTestDataForLegacyMode))]
    public void MapCode_LegacyMode_CharInput_ReturnValidMappedCode(MapCodeTestData data)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = true
        };

        // Get MapCode function
        var mapCodeFunc = typeof(EscapeSequenceParser)
            .GetMethod("MapCode", BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(mapCodeFunc);

        var result = mapCodeFunc.Invoke(parser, [data.RawCode]);
        Assert.NotNull(result);
        var actual = (byte)result;

        Assert.Equal(data.ExpectedCode, actual);
    }

    public static TheoryData<MapCodeTestData> GetMapCodeTestDataForUnicodeMode()
    {
        var data = new TheoryData<MapCodeTestData>();

        // Unicode characters
        data.Add(new MapCodeTestData('\u00A0', 0xA0));
        data.Add(new MapCodeTestData('\uFFFF', 0xA0));
        data.Add(new MapCodeTestData('\u5948', 0xA0));

        // All characters in C0, GL and C1 areas
        for (var code = '\u0000'; code <= '\u009F'; ++code)
        {
            data.Add(new MapCodeTestData(code, (byte)code));
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetMapCodeTestDataForUnicodeMode))]
    public void MapCode_UnicodeMode_CharInput_ReturnValidMappedCode(MapCodeTestData data)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = false
        };

        // Get MapCode function
        var mapCodeFunc = typeof(EscapeSequenceParser)
            .GetMethod("MapCode", BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(mapCodeFunc);

        var result = mapCodeFunc.Invoke(parser, [data.RawCode]);
        Assert.NotNull(result);
        var actual = (byte)result;

        Assert.Equal(data.ExpectedCode, actual);
    }

    public static TheoryData<ParseTestData> GetPrintableInputTestDataForLegacyMode()
    {
        var data = new TheoryData<ParseTestData>();

        // Characters in GL area
        for (var code = '\u0020'; code <= '\u007F'; ++code)
        {
            AddData(code.ToString());
        }

        // Characters in GR area
        for (var code = '\u00A0'; code <= '\u00FF'; ++code)
        {
            AddData(code.ToString());
        }

        // Consecutive valid characters
        AddData("The quick brown fox jumps over the lazy dog.");
        AddData("\u00AE\u00F9\u00E4\u00E0\u00E8\u00A9\u00BB\u00F5\u00AE\u00DC\u00C8");
        AddData("\u00DB\u007A\u00CB\u00F0\u0068\u0061\u006A\u00E6\u0040\u0031");
        AddData("\u007F\u007F\u00FF\u00A0\u00FF\u0020\u00FF\u007F");

        return data;

        // Helper function(s)
        // ------------------
        // Add a string to test data sets
        void AddData(string text)
        {
            data.Add(new ParseTestData(text, [new ActionRecord("print", text)]));
        }
    }

    [Theory]
    [MemberData(nameof(GetPrintableInputTestDataForLegacyMode))]
    public void Parse_LegacyMode_PrintableInput_CallPrintHandler(ParseTestData data)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = true
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }

    public static TheoryData<ParseTestData> GetPrintableInputTestDataFoUnicodeMode()
    {
        var data = new TheoryData<ParseTestData>();

        // Characters in GL area
        for (var code = '\u0020'; code <= '\u007F'; ++code)
        {
            AddData(code.ToString());
        }

        // Single Unicode character
        AddData("\u00A0");
        AddData("\uFFFF");
        AddData("\u5948");
        AddData("\uD616");

        // Consecutive valid characters
        AddData("The quick brown fox jumps over the lazy dog.");
        AddData("\u46CD\u3635\uF068\uB935\u0801\u2F68\uF30C\uA176");
        AddData("\uC65B\u0023\u0FB7\u04B5\uE5D1\uBBFB\u007C\u8549\u6F38\u1246\u006E");
        AddData("\uFFFF\u0020\u0020\uFFFF\uFFFF\u007F\u00A0\u007F");

        return data;

        // Helper function(s)
        // ----------------
        // Add a string to test data sets
        void AddData(string text)
        {
            data.Add(new ParseTestData(text, [new ActionRecord("print", text)]));
        }
    }

    [Theory]
    [MemberData(nameof(GetPrintableInputTestDataFoUnicodeMode))]
    public void Parse_UnicodeMode_PrintableInput_CallPrintHandler(ParseTestData data)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = false
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }

    public static TheoryData<ParseTestData> GetControlCharactersInputTestData()
    {
        var data = new TheoryData<ParseTestData>();
        var executableC0 = C0.All.Except([C0.ESC]).ToArray();
        var executableC1 = C1.All.Except([C1.DCS, C1.SOS, C1.CSI, C1.ST, C1.OSC, C1.PM, C1.APC]).ToArray();

        // C0 characters
        foreach (var code in executableC0)
        {
            AddData(code.ToString());
        }

        // C1 characters
        foreach (var code in executableC1)
        {
            AddData(code.ToString());
        }

        // C0 and C1 controls sequences
        AddData("\u0006\u000A\u0018");
        AddData("\u0081\u008E\u0095\u008F");
        AddData("\u0082\u008F\u0083\u000B\u0094\u0009");
        AddData("\u0014\u0011\u0092\u0003\u001E\u008A");

        return data;

        // Helper function(s)
        // ----------------
        // Add a string to test data sets
        void AddData(string text)
        {
            data.Add(new ParseTestData(false, text, text.Select(c => new ActionRecord("execute", c)).ToList()));
            data.Add(new ParseTestData(true, text, text.Select(c => new ActionRecord("execute", c)).ToList()));
        }
    }

    [Theory]
    [MemberData(nameof(GetControlCharactersInputTestData))]
    public void Parse_ControlCharactersInput_CallExecuteHandler(ParseTestData data)
    {
        Assert.NotNull(data.LegacyMode);

        var parser = new EscapeSequenceParser
        {
            LegacyMode = data.LegacyMode!.Value
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }

    public static TheoryData<ParseTestData> GetEscapeSequenceWithOneIntermediateTestData()
    {
        var data = new TheoryData<ParseTestData>();
        var validIntermediate = Enumerable.Range('\u0030', '\u007E' - '\u0030' + 1)
            .Except(['\u0050', '\u0058', '\u0059', '\u005B', '\u005D', '\u005E', '\u005F'])
            .Select(c => (char)c)
            .ToArray();

        foreach (var code in validIntermediate)
        {
            data.Add(new ParseTestData(false, C0.ESC + code.ToString(), [new ActionRecord("escape", code, "")]));
            data.Add(new ParseTestData(true, C0.ESC + code.ToString(), [new ActionRecord("escape", code, "")]));
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetEscapeSequenceWithOneIntermediateTestData))]
    public void Parse_EscapeSequenceWithOneIntermediateInput_CallEscapeHandlerWithEmptyCollect(ParseTestData data)
    {
        Assert.NotNull(data.LegacyMode);

        var parser = new EscapeSequenceParser
        {
            LegacyMode = data.LegacyMode!.Value
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }

    public static TheoryData<ParseTestData> GetEscapeSequenceWithManyIntermediatesTestData()
    {
        var data = new TheoryData<ParseTestData>();
        var validIntermediate = Enumerable.Range('\u0020', '\u002F' - '\u0020' + 1)
            .Select(c => (char)c)
            .ToArray();

        foreach (var code in validIntermediate)
        {
            AddData('\u0030', code.ToString());
            AddData('\u007E', code.ToString());
            AddData('\u0059', code.ToString());
        }

        // Collect with many characters
        AddData('\u0030', "\u002B\u0023\u002A\u0026\u002D\u0020");
        AddData('\u007E', "\u002B\u0023\u002A\u0026\u002D\u0020");
        AddData('\u0030', "\u0020\u0024\u002D\u0029\u002F\u002D");
        AddData('\u007E', "\u0020\u0024\u002D\u0029\u002F\u002D");

        return data;

        // Helper function(s)
        // ------------------
        // Add an escape sequence to test data sets
        void AddData(char code, string collect)
        {
            data.Add(new ParseTestData(false, C0.ESC + collect + code,
                [new ActionRecord("escape", code, collect)]));
            data.Add(new ParseTestData(true, C0.ESC + collect + code,
                [new ActionRecord("escape", code, collect)]));
        }
    }

    [Theory]
    [MemberData(nameof(GetEscapeSequenceWithManyIntermediatesTestData))]
    public void Parse_EscapeSequenceWithManyIntermediatesInput_CallEscapeHandlerWithCollect(ParseTestData data)
    {
        Assert.NotNull(data.LegacyMode);

        var parser = new EscapeSequenceParser
        {
            LegacyMode = data.LegacyMode!.Value
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }

    public static TheoryData<ParseTestData> GetCsiSequenceWithoutParamAndCollectTestData()
    {
        var data = new TheoryData<ParseTestData>();

        for (var code = '\u0040'; code <= '\u007E'; ++code)
        {
            AddData(code);
        }

        return data;

        // Helper function(s)
        // ------------------
        // Add a CSI sequence to test data sets
        void AddData(char code)
        {
            var text = C0.ESC + "\u005B" + code;
            data.Add(new ParseTestData(false, text, [new ActionRecord("csi", code, "", [0])]));
            data.Add(new ParseTestData(true, text, [new ActionRecord("csi", code, "", [0])]));

            text = C1.CSI + code.ToString();
            data.Add(new ParseTestData(false, text, [new ActionRecord("csi", code, "", [0])]));
            data.Add(new ParseTestData(true, text, [new ActionRecord("csi", code, "", [0])]));
        }
    }

    [Theory]
    [MemberData(nameof(GetCsiSequenceWithoutParamAndCollectTestData))]
    public void Parse_CsiSequenceWithoutParamAndCollectInput_CallCsiHandlerWithEmptyParamAndCollect(ParseTestData data)
    {
        Assert.NotNull(data.LegacyMode);

        var parser = new EscapeSequenceParser
        {
            LegacyMode = data.LegacyMode!.Value
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }

    public static TheoryData<ParseTestData> GetCsiSequenceWithCollectTestData()
    {
        var data = new TheoryData<ParseTestData>();

        for (var code = '\u0020'; code <= '\u002F'; ++code)
        {
            AddData(code.ToString());
        }

        // Collect with many characters
        AddData("\u0020\u0028\u0022\u002B\u0023\u002C");
        AddData("\u0025\u0028\u0026\u002E\u0021\u0027\u002F\u0020");

        return data;

        // Helper function(s)
        // ------------------
        // Add a CSI sequence to test data sets
        void AddData(string collect)
        {
            var text = C0.ESC + "\u005B" + collect + "\u0040";
            data.Add(new ParseTestData(false, text, [new ActionRecord("csi", '\u0040', collect, [0])]));
            data.Add(new ParseTestData(true, text, [new ActionRecord("csi", '\u0040', collect, [0])]));

            text = C1.CSI + collect + "\u0040";
            data.Add(new ParseTestData(false, text, [new ActionRecord("csi", '\u0040', collect, [0])]));
            data.Add(new ParseTestData(true, text, [new ActionRecord("csi", '\u0040', collect, [0])]));

            text = C0.ESC + "\u005B" + collect + "\u007E";
            data.Add(new ParseTestData(false, text, [new ActionRecord("csi", '\u007E', collect, [0])]));
            data.Add(new ParseTestData(true, text, [new ActionRecord("csi", '\u007E', collect, [0])]));

            text = C1.CSI + collect + "\u007E";
            data.Add(new ParseTestData(false, text, [new ActionRecord("csi", '\u007E', collect, [0])]));
            data.Add(new ParseTestData(true, text, [new ActionRecord("csi", '\u007E', collect, [0])]));
        }
    }

    [Theory]
    [MemberData(nameof(GetCsiSequenceWithCollectTestData))]
    public void Parse_CsiSequenceWithCollectInput_CallCsiHandlerWithCollectAndEmptyParam(ParseTestData data)
    {
        Assert.NotNull(data.LegacyMode);

        var parser = new EscapeSequenceParser
        {
            LegacyMode = data.LegacyMode!.Value
        };
        var handler = new MockHandler(parser);

        parser.Parse(data.Text);
        var actual = handler.Actions;

        Assert.Equal(data.ExpectedActions, actual);
    }
}