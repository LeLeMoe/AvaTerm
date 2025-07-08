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

        private MockDcsHandler _dcsHandler;
        private MockAuxStringHandler _oscHandler;
        private MockAuxStringHandler _sosHandler;
        private MockAuxStringHandler _pmHandler;
        private MockAuxStringHandler _apcHandler;

        private List<ActionRecord> _actions = [];

        public List<ActionRecord> Actions => _actions;

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
            _actions.Add(record);
        }

        public MockHandler(EscapeSequenceParser parser)
        {
            _dcsHandler = new MockDcsHandler(Record);
            _oscHandler = new MockAuxStringHandler("osc", Record);
            _sosHandler = new MockAuxStringHandler("sos", Record);
            _pmHandler = new MockAuxStringHandler("pm", Record);
            _apcHandler = new MockAuxStringHandler("apc", Record);

            parser.PrintHandler = PrintHandler;
            parser.ExecuteHandler = ExecuteHandler;
            parser.EscapeHandler = EscapeHandler;
            parser.CsiHandler = CsiHandler;
            parser.DcsHandler = _dcsHandler;
            parser.OscHandler = _oscHandler;
            parser.SosHandler = _sosHandler;
            parser.PmHandler = _pmHandler;
            parser.ApcHandler = _apcHandler;
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
        var resultAction = resultType.GetField("Item1");
        var resultState = resultType.GetField("Item2");
        Assert.NotNull(resultAction);
        Assert.NotNull(resultState);

        // Check results
        Assert.NotEqual(resultAction.Name, invalidAction.Name);
        Assert.NotEqual(resultState.Name, invalidState.Name);
    }

    public static TheoryData<char> GetMapCodeAnyInputTestData()
    {
        var data = new TheoryData<char>();

        for (var code = 0x0000; code <= 0xFFFF; ++code)
        {
            data.Add((char)code);
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(GetMapCodeAnyInputTestData))]
    public void MapCode_AnyInput_ReturnCodeInValidRange(char code)
    {
        var parser = new EscapeSequenceParser();

        // Get MapCode function
        var mapCodeFunc = typeof(EscapeSequenceParser)
            .GetMethod("MapCode", BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(mapCodeFunc);

        var result = mapCodeFunc.Invoke(parser, [code]);
        Assert.NotNull(result);
        var resultValue = (byte)result;

        Assert.InRange(resultValue, 0x00, 0xA0);
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