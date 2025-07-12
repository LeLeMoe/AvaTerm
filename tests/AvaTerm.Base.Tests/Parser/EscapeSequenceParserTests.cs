using System.Reflection;
using AvaTerm.Data;
using AvaTerm.Parser;
using static AvaTerm.Parser.EscapeSequenceParser;
using C0 = AvaTerm.Data.C0Controls;
using C1 = AvaTerm.Data.C1Controls;

namespace AvaTerm.Base.Tests.Parser;

public class EscapeSequenceParserTests
{
    private readonly struct ActionRecord : IEquatable<ActionRecord>
    {
        private readonly string _action;
        private readonly char? _code;
        private readonly string? _collect;
        private readonly int[]? _param;
        private readonly string? _text;

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
    }

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

    public class TransitionTableCompletenessTestData
    {
        private readonly string _stateName = "";

        public byte Code { get; private set; }
        public int State { get; private set; }

        public TransitionTableCompletenessTestData()
        {
        }

        public TransitionTableCompletenessTestData(byte code, int state, string stateName)
        {
            Code = code;
            State = state;
            _stateName = stateName;
        }

        public override string ToString()
        {
            return $"(code: {Code}, state: {_stateName})";
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

    public static TheoryData<char, byte> GetMapCodeTestDataForLegacyMode()
    {
        var data = new TheoryData<char, byte>();

        // Invalid Unicode characters
        data.Add('\u0100', 0x00);
        data.Add('\uFFFF', 0x00);
        data.Add('\u5948', 0x00);

        // All characters in GR area
        for (var code = '\u00A0'; code <= '\u00FF'; ++code)
        {
            data.Add(code, (byte)(code - 0x0080));
        }

        // All characters in C0, GL and C1 areas
        for (var code = '\u0000'; code <= '\u009F'; ++code)
        {
            data.Add(code, (byte)code);
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetMapCodeTestDataForLegacyMode))]
    public void MapCode_CharInputInLegacyMode_ReturnValidMappedCode(char rawCode, byte mappedCode)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = true
        };

        // Get MapCode function
        var mapCodeFunc = typeof(EscapeSequenceParser)
            .GetMethod("MapCode", BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(mapCodeFunc);

        var result = mapCodeFunc.Invoke(parser, [rawCode]);
        Assert.NotNull(result);
        var resultValue = (byte)result;

        Assert.Equal(mappedCode, resultValue);
    }

    public static TheoryData<char, byte> GetMapCodeTestDataForUnicodeMode()
    {
        var data = new TheoryData<char, byte>();

        // Unicode characters
        data.Add('\u00A0', 0xA0);
        data.Add('\uFFFF', 0xA0);
        data.Add('\u5948', 0xA0);

        // All characters in C0, GL and C1 areas
        for (var code = '\u0000'; code <= '\u009F'; ++code)
        {
            data.Add(code, (byte)code);
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetMapCodeTestDataForUnicodeMode))]
    public void MapCode_CharInputInUnicodeMode_ReturnValidMappedCode(char rawCode, byte mappedCode)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = false
        };

        // Get MapCode function
        var mapCodeFunc = typeof(EscapeSequenceParser)
            .GetMethod("MapCode", BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(mapCodeFunc);

        var result = mapCodeFunc.Invoke(parser, [rawCode]);
        Assert.NotNull(result);
        var resultValue = (byte)result;

        Assert.Equal(mappedCode, resultValue);
    }

    public static TheoryData<string> GetPrintableInputTestDataForLegacyMode()
    {
        var data = new TheoryData<string>();

        // Characters in GL area
        for (var code = '\u0020'; code <= '\u007F'; ++code)
        {
            data.Add(code.ToString());
        }

        // Characters in GR area
        for (var code = '\u00A0'; code <= '\u00FF'; ++code)
        {
            data.Add(code.ToString());
        }

        // Consecutive valid characters
        data.Add("The quick brown fox jumps over the lazy dog.");
        data.Add("\u00AE\u00F9\u00E4\u00E0\u00E8\u00A9\u00BB\u00F5\u00AE\u00DC\u00C8");
        data.Add("\u00DB\u007A\u00CB\u00F0\u0068\u0061\u006A\u00E6\u0040\u0031");
        data.Add("\u007F\u007F\u00FF\u00A0\u00FF\u0020\u00FF\u007F");

        return data;
    }

    [Theory]
    [MemberData(nameof(GetPrintableInputTestDataForLegacyMode))]
    public void Parse_PrintableInputInLegacyMode_CallPrintAction(string text)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = true
        };
        var handler = new MockHandler(parser);

        parser.Parse(text);
        var actual = handler.Actions;

        var expected = new List<ActionRecord> { new ActionRecord("print", text) };
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string> GetPrintableInputTestDataFoUnicodeMode()
    {
        var data = new TheoryData<string>();

        // Characters in GL area
        for (var code = '\u0020'; code <= '\u007F'; ++code)
        {
            data.Add(code.ToString());
        }

        // Single Unicode character
        data.Add("\u00A0");
        data.Add("\uFFFF");
        data.Add("\u5948");
        data.Add("\uD616");

        // Consecutive valid characters
        data.Add("The quick brown fox jumps over the lazy dog.");
        data.Add("\u46CD\u3635\uF068\uB935\u0801\u2F68\uF30C\uA176");
        data.Add("\uC65B\u0023\u0FB7\u04B5\uE5D1\uBBFB\u007C\u8549\u6F38\u1246\u006E");
        data.Add("\uFFFF\u0020\u0020\uFFFF\uFFFF\u007F\u00A0\u007F");

        return data;
    }

    [Theory]
    [MemberData(nameof(GetPrintableInputTestDataFoUnicodeMode))]
    public void Parse_PrintableInputInUnicodeMode_CallPrintAction(string text)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = false
        };
        var handler = new MockHandler(parser);

        parser.Parse(text);
        var actual = handler.Actions;

        var expected = new List<ActionRecord> { new ActionRecord("print", text) };
        Assert.Equal(expected, actual);
    }

    public static TheoryData<bool, string> GetControlCharactersInputTestData()
    {
        var data = new TheoryData<bool, string>();
        var executableC0 = C0.All.Except([C0.ESC]).ToArray();
        var executableC1 = C1.All.Except([C1.DCS, C1.SOS, C1.CSI, C1.ST, C1.OSC, C1.PM, C1.APC]).ToArray();

        // C0 characters
        foreach (var code in executableC0)
        {
            data.Add(false, code.ToString());
            data.Add(true, code.ToString());
        }

        // C1 characters
        foreach (var code in executableC1)
        {
            data.Add(false, code.ToString());
            data.Add(true, code.ToString());
        }

        // TODO: C0 & C1 controls sequences

        return data;
    }

    [Theory]
    [MemberData(nameof(GetControlCharactersInputTestData))]
    public void Parse_ControlCharactersInput_CallExecuteAction(bool legacyMode, string codes)
    {
        var parser = new EscapeSequenceParser
        {
            LegacyMode = legacyMode
        };
        var handler = new MockHandler(parser);

        parser.Parse(codes);
        var actual = handler.Actions;

        var expected = codes.Select(code => new ActionRecord("execute", code)).ToList();
        Assert.Equal(expected, actual);
    }
}