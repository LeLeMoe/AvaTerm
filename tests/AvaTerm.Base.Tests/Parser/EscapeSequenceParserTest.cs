using System.Reflection;
using AvaTerm.Data;
using AvaTerm.Parser;
using Moq;
using static AvaTerm.Parser.EscapeSequenceParser;

namespace AvaTerm.Base.Tests.Parser;

public class EscapeSequenceParserTest
{
    private interface IDcsHandlerAdapter
    {
        void Hook(char code, string collect, int[] parameters);
        void Put(string text);
        void Unhook();
    }

    private interface IAuxStringHandlerAdapter
    {
        void Start();
        void Put(string text);
        void End();
    }

    private class DcsHandlerAdapter(Mock<IDcsHandlerAdapter> mockHandler) : IDcsHandler
    {
        public void Hook(char code, string collect, int[] parameters)
        {
            mockHandler.Object.Hook(code, collect, parameters);
        }

        public void Put(ReadOnlySpan<char> text)
        {
            mockHandler.Object.Put(text.ToString());
        }

        public void Unhook()
        {
            mockHandler.Object.Unhook();
        }
    }

    private class AuxStringHandlerAdapter(Mock<IAuxStringHandlerAdapter> mockHandler) : IAuxStringHandler
    {
        public void Start()
        {
            mockHandler.Object.Start();
        }

        public void Put(ReadOnlySpan<char> text)
        {
            mockHandler.Object.Put(text.ToString());
        }

        public void End()
        {
            mockHandler.Object.End();
        }
    }

    private readonly EscapeSequenceParser _parser = new();
    private readonly Mock<Action<string>> _printHandler = new(MockBehavior.Strict);
    private readonly Mock<Action<char>> _executeHandler = new(MockBehavior.Strict);
    private readonly Mock<Action<char, string>> _escapeHandler = new(MockBehavior.Strict);
    private readonly Mock<Action<char, string, int[]>> _csiHandler = new(MockBehavior.Strict);
    private readonly Mock<IDcsHandlerAdapter> _dcsHandler = new(MockBehavior.Strict);
    private readonly Mock<IAuxStringHandlerAdapter> _oscHandler = new(MockBehavior.Strict);
    private readonly Mock<IAuxStringHandlerAdapter> _sosHandler = new(MockBehavior.Strict);
    private readonly Mock<IAuxStringHandlerAdapter> _pmHandler = new(MockBehavior.Strict);
    private readonly Mock<IAuxStringHandlerAdapter> _apcHandler = new(MockBehavior.Strict);

    private void PrintHandlerAdapter(ReadOnlySpan<char> text)
    {
        _printHandler.Object(text.ToString());
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

    public EscapeSequenceParserTest()
    {
        _parser.PrintHandler = PrintHandlerAdapter;
        _parser.ExecuteHandler = _executeHandler.Object;
        _parser.EscapeHandler = _escapeHandler.Object;
        _parser.CsiHandler = _csiHandler.Object;
        _parser.DcsHandler = new DcsHandlerAdapter(_dcsHandler);
        _parser.OscHandler = new AuxStringHandlerAdapter(_oscHandler);
        _parser.SosHandler = new AuxStringHandlerAdapter(_sosHandler);
        _parser.PmHandler = new AuxStringHandlerAdapter(_pmHandler);
        _parser.ApcHandler = new AuxStringHandlerAdapter(_apcHandler);
    }

    #region Transition Table Completeness Test

    public static TheoryData<byte, int> TransitionTableCompletenessTestData()
    {
        var data = new TheoryData<byte, int>();

        // Get all valid states
        var validStates = GetAllStates().Where(s => s.Name != "Invalid");

        // Enumerate all states and code range from 0x00 to 0xA0
        foreach (var state in validStates)
        {
            for (byte code = 0x00; code <= 0xA0; ++code)
            {
                var stateValue = state.GetValue(null);
                Assert.NotNull(stateValue);
                data.Add(code, (int)stateValue);
            }
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(TransitionTableCompletenessTestData))]
    public void TransitionTable_CheckCompleteness(byte code, int state)
    {
        var parser = new EscapeSequenceParser();

        // Since the transition table is a private member of EscapeSequenceParser,
        // we need to access it via reflection.

        // Get invalid states
        var invalidStates = GetAllStates().FirstOrDefault(s => s.Name == "Invalid");
        Assert.NotNull(invalidStates);

        // Get invalid actions
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
        var result = transitionFunc.Invoke(transitionTable, [code, state]);
        Assert.NotNull(result);

        // Deconstruct the returned tuple
        var resultType = result.GetType();
        var resultAction = resultType.GetField("Item1");
        var resultState = resultType.GetField("Item2");
        Assert.NotNull(resultAction);
        Assert.NotNull(resultState);

        var resultActionValue = resultAction.GetValue(result);
        var resultStateValue = resultState.GetValue(result);
        Assert.NotNull(resultActionValue);
        Assert.NotNull(resultStateValue);

        var resultActionName = Enum.GetName(resultAction.FieldType, resultActionValue);
        var resultStateName = Enum.GetName(resultState.FieldType, resultStateValue);
        Assert.NotNull(resultActionName);
        Assert.NotNull(resultStateName);

        // Check results
        Assert.NotEqual(invalidAction.Name, resultActionName);
        Assert.NotEqual(invalidStates.Name, resultStateName);
    }

    #endregion

    #region MapCode Method Test

    public static TheoryData<bool, char, byte> MapCodeTestData()
    {
        var data = new TheoryData<bool, char, byte>();

        // All characters in C0, GL and C1 areas
        for (var code = '\x00'; code <= '\x9F'; ++code)
        {
            data.Add(true, code, (byte)code);
            data.Add(false, code, (byte)code);
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            // In legacy mode, all characters in GR area will be mapped to GL area
            data.Add(true, code, (byte)(code - '\x80'));

            // In Unicode mode, all characters in GR area will be mapped to 0xA0
            data.Add(false, code, 0xA0);
        }

        // Some Unicode characters
        // -----------------------
        // In legacy mode, all Unicode characters are invalid, which will be mapped to NUL
        data.Add(true, '\u0100', 0x00);
        data.Add(true, '\u5948', 0x00);
        data.Add(true, '\uFFFF', 0x00);
        // In Unicode mode, all Unicode characters will be mapped to 0xA0
        data.Add(false, '\u0100', 0xA0);
        data.Add(false, '\u5948', 0xA0);
        data.Add(false, '\uFFFF', 0xA0);

        return data;
    }

    [Theory]
    [MemberData(nameof(MapCodeTestData))]
    public void MapCode_AnyCharacter_ReturnValidMappedCode(bool legacyMode, char rawCode, byte expectedCode)
    {
        _parser.LegacyMode = legacyMode;

        // Get MapCode function
        var mapCodeFunc = typeof(EscapeSequenceParser)
            .GetMethod("MapCode", BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(mapCodeFunc);

        var result = mapCodeFunc.Invoke(_parser, [rawCode]);
        Assert.NotNull(result);
        var actual = (byte)result;

        Assert.Equal(expectedCode, actual);
    }

    #endregion

    #region Parse Printable Text Test

    public static TheoryData<bool, string> PrintableTextTestData()
    {
        var data = new TheoryData<bool, string>();

        // All characters in GL area
        for (var code = '\x20'; code <= '\x7F'; ++code)
        {
            data.Add(true, code.ToString());
            data.Add(false, code.ToString());
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            data.Add(true, code.ToString());
            data.Add(false, code.ToString());
        }

        // Some Unicode characters
        data.Add(false, "\u0100");
        data.Add(false, "\u5948");
        data.Add(false, "\uFFFF");

        // Some consecutive printable sequences
        var text = "The quick brown fox jumps over the lazy dog.";
        data.Add(true, text);
        data.Add(false, text);
        // TODO

        return data;
    }

    [Theory]
    [MemberData(nameof(PrintableTextTestData))]
    public void Parse_PrintableText_CallPrintHandlerOnce(bool legacyMode, string input)
    {
        var sequence = new MockSequence();
        _printHandler.InSequence(sequence).Setup(h => h(It.Is<string>(s => s == input)));

        _parser.LegacyMode = legacyMode;
        _parser.Parse(input);
    }

    #endregion

    #region Parse Executable Text Test

    public static TheoryData<bool, string> ExecutableTextTestData()
    {
        var data = new TheoryData<bool, string>();

        // All executable C0 controls
        foreach (var code in C0.All.Except([C0.ESC]))
        {
            data.Add(true, code.ToString());
            data.Add(false, code.ToString());
        }

        // All executable C1 controls
        foreach (var code in C1.All.Except([C1.DCS, C1.SOS, C1.OSC, C1.PM, C1.APC]))
        {
            data.Add(true, code.ToString());
            data.Add(false, code.ToString());
        }

        // TODO

        return data;
    }

    [Theory]
    [MemberData(nameof(ExecutableTextTestData))]
    public void Parse_ExecutableText_CallExecuteHandlerCorrectly(bool legacyMode, string input)
    {
        var sequence = new MockSequence();

        foreach (var character in input)
        {
            _executeHandler.InSequence(sequence).Setup(h => h(It.Is<char>(c => c == character)));
        }

        _parser.LegacyMode = legacyMode;
        _parser.Parse(input);
    }

    #endregion

    #region Parse Single Function Escape Sequence Test

    public static TheoryData<bool, string, char, string> EscapeSequenceTestData()
    {
        var data = new TheoryData<bool, string, char, string>();
        var validIntermediates = Enumerable.Range('\x30', '\x7E' - '\x30')
            .Except(['\x50', '\x58', '\x5B', '\x5D', '\x5E', '\x5F']);
        var text = "";

        // All valid intermediates without collect
        foreach (char code in validIntermediates)
        {
            text = "\x1B" + code;
            data.Add(true, text, code, "");
            data.Add(false, text, code, "");
        }

        // TODO

        return data;
    }

    [Theory]
    [MemberData(nameof(EscapeSequenceTestData))]
    public void Parse_EscapeSequence_CallEscapeHandlerOnce(bool legacyMode, string input, char expectedCode,
        string expectedCollect)
    {
        var sequence = new MockSequence();
        _escapeHandler.InSequence(sequence)
            .Setup(h => h(It.Is<char>(c => c == expectedCode), It.Is<string>(c => c == expectedCollect)));

        _parser.LegacyMode = legacyMode;
        _parser.Parse(input);
    }

    #endregion

    #region Parse CSI Sequence Test

    public static TheoryData<string, char, string, int[]> CsiSequenceTestData()
    {
        var data = new TheoryData<string, char, string, int[]>();

        // Simple CSI sequence without any collect and parameters
        for (var code = '\x40'; code <= '\x7E'; ++code)
        {
            data.Add(C0.ESC + "[" + code, code, "", [0]);
            data.Add(C1.CSI + code.ToString(), code, "", [0]);
        }

        // CSI sequence with single parameter
        for (var num = 0; num <= 9; ++num)
        {
            data.Add(C0.ESC + "[" + num + "A", 'A', "", [num]);
            data.Add(C1.CSI + num.ToString() + "A", 'A', "", [num]);
        }

        data.Add(C0.ESC + "[65535D", 'D', "", [65535]);
        data.Add(C1.CSI + "824M", 'M', "", [824]);

        // Csi sequence with multiple parameters
        data.Add(C0.ESC + "[185;62S", 'S', "", [185, 62]);
        data.Add(C1.CSI + "595;1345;37Z", 'Z', "", [595, 1345, 37]);

        // Csi sequence with collect
        for (var collect = '\x20'; collect <= '\x2F'; ++collect)
        {
            data.Add(C0.ESC + "[" + collect + "F", 'F', collect.ToString(), [0]);
            data.Add(C1.CSI + collect.ToString() + "F", 'F', collect.ToString(), [0]);
        }

        for (var collect = '\x3C'; collect <= '\x3F'; ++collect)
        {
            data.Add(C0.ESC + "[" + collect + "w", 'w', collect.ToString(), [0]);
            data.Add(C1.CSI + collect.ToString() + "w", 'w', collect.ToString(), [0]);
        }

        data.Add(C0.ESC + "[/( !k", 'k', "/( !", [0]);
        data.Add(C1.CSI + "$+h", 'h', "$+", [0]);

        // Csi sequence with parameters and collect
        data.Add(C0.ESC + "[58 @", '@', " ", [58]);
        data.Add(C1.CSI + ">2T", 'T', ">", [2]);
        data.Add(C0.ESC + "[?45;2001l", 'l', "?", [45, 2001]);
        data.Add(C1.CSI + "114;514*%{", '{', "*%", [114, 514]);

        // Csi sequence with empty parameters
        data.Add(C0.ESC + "[;19g", 'g', "", [0, 19]);
        data.Add(C1.CSI + "8;;1~", '~', "", [8, 0, 1]);
        data.Add(C0.ESC + "[3;;,'I", 'I', ",'", [3, 0, 0]);
        data.Add(C1.CSI + ";;;-(q", 'q', "-(", [0, 0, 0, 0]);

        return data;
    }

    [Theory]
    [MemberData(nameof(CsiSequenceTestData))]
    public void Parse_CsiSequence_CallCsiHandlerCorrectly(string input, char expectedCode, string expectedCollect,
        int[] expectedParam)
    {
        var sequence = new MockSequence();
        _csiHandler.InSequence(sequence).Setup(h => h(It.Is<char>(c => c == expectedCode),
            It.Is<string>(s => s == expectedCollect), It.Is<int[]>(a => a.SequenceEqual(expectedParam))));

        _parser.Parse(input);
    }

    #endregion
}