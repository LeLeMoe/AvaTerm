using System.Reflection;
using AvaTerm.Parser;
using Xunit.Abstractions;
using static AvaTerm.Parser.EscapeSequenceParser;

namespace AvaTerm.Base.UnitTests.Parser;

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


    public class TransitionTableCompletenessTestData : IXunitSerializable
    {
        private string _stateName = "";

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

        public void Deserialize(IXunitSerializationInfo info)
        {
            Code = info.GetValue<byte>("Code");
            State = info.GetValue<int>("State");
            _stateName = info.GetValue<string>("StateName");
        }

        public void Serialize(IXunitSerializationInfo info)
        {
            info.AddValue("Code", Code);
            info.AddValue("State", State);
            info.AddValue("StateName", _stateName);
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
        return new TheoryData<char, byte>
        {
            // Invalid Unicode characters
            { '\u0100', 0x00 },
            { '\uFFFF', 0x00 },
            { '\uF90C', 0x00 },
            { '\u306A', 0x00 },

            // Characters in GR area
            { '\u00A0', 0x20 },
            { '\u00FF', 0x7F },
            { '\u00C1', 0x41 },
            { '\u00FA', 0x7A },

            // Characters in C0, C1 and GL areas
            { '\u0000', 0x00 },
            { '\u001F', 0x1F },
            { '\u0009', 0x09 },
            { '\u001B', 0x1B },
            { '\u0080', 0x80 },
            { '\u009F', 0x9F },
            { '\u0084', 0x84 },
            { '\u009B', 0x9B },
            { '\u0020', 0x20 },
            { '\u007F', 0x7F },
            { '\u0041', 0x41 },
            { '\u007A', 0x7A },
        };
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
        return new TheoryData<char, byte>
        {
            // Unicode characters
            { '\u00A0', 0xA0 },
            { '\uFFFF', 0xA0 },
            { '\u00BC', 0xA0 },
            { '\u306A', 0xA0 },

            // Characters in C0, C1 and GL areas
            { '\u0000', 0x00 },
            { '\u001F', 0x1F },
            { '\u0009', 0x09 },
            { '\u001B', 0x1B },
            { '\u0080', 0x80 },
            { '\u009F', 0x9F },
            { '\u0084', 0x84 },
            { '\u009B', 0x9B },
            { '\u0020', 0x20 },
            { '\u007F', 0x7F },
            { '\u0041', 0x41 },
            { '\u007A', 0x7A },
        };
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

    [Theory]
    [InlineData("\u0020")]
    [InlineData("\u007F")]
    [InlineData("Hello, World!")]
    public void Parse_PrintableInput_CallPrintAction(string text)
    {
        var parser = new EscapeSequenceParser();
        var handler = new MockHandler(parser);

        parser.Parse(text);
        var result = handler.Actions;

        var actual = new List<ActionRecord> { new ActionRecord("print", text) };
        Assert.True(result.SequenceEqual(actual));
    }
}