using System.Reflection;
using AvaTerm.Data;
using AvaTerm.Parser;
using NSubstitute;
using static AvaTerm.Parser.EscapeSequenceParser;

namespace AvaTerm.Base.Tests.Parser;

public class EscapeSequenceParserTest
{
    // Since ReadOnlySpan<> cannot be used as a type parameter, we need to apply additional wrappers to convert
    // ReadOnlySpan<char> to string before providing it to mock.
    private class DcsHandlerWrapper(IDcsHandlerWrapper mockHandler) : IDcsHandler
    {
        public void Hook(char code, string collect, int[] parameters)
        {
            mockHandler.Hook(code, collect, parameters);
        }

        public void Put(ReadOnlySpan<char> text)
        {
            mockHandler.Put(text.ToString());
        }

        public void Unhook()
        {
            mockHandler.Unhook();
        }
    }

    private class AuxStringHandlerWrapper(IAuxStringHandlerWrapper mockHandler) : IAuxStringHandler
    {
        public void Start()
        {
            mockHandler.Start();
        }

        public void Put(ReadOnlySpan<char> text)
        {
            mockHandler.Put(text.ToString());
        }

        public void End()
        {
            mockHandler.End();
        }
    }

    public interface IDcsHandlerWrapper
    {
        void Hook(char code, string collect, int[] parameters);
        void Put(string text);
        void Unhook();
    }

    public interface IAuxStringHandlerWrapper
    {
        void Start();
        void Put(string text);
        void End();
    }

    private readonly EscapeSequenceParser _parser = new();
    private readonly Action<string> _printHandler = Substitute.For<Action<string>>();
    private readonly Action<char> _executeHandler = Substitute.For<Action<char>>();
    private readonly Action<char, string> _escapeHandler = Substitute.For<Action<char, string>>();
    private readonly Action<char, string, int[]> _csiHandler = Substitute.For<Action<char, string, int[]>>();
    private readonly IDcsHandlerWrapper _dcsHandler = Substitute.For<IDcsHandlerWrapper>();
    private readonly IAuxStringHandlerWrapper _oscHandler = Substitute.For<IAuxStringHandlerWrapper>();
    private readonly IAuxStringHandlerWrapper _sosHandler = Substitute.For<IAuxStringHandlerWrapper>();
    private readonly IAuxStringHandlerWrapper _pmHandler = Substitute.For<IAuxStringHandlerWrapper>();
    private readonly IAuxStringHandlerWrapper _apcHandler = Substitute.For<IAuxStringHandlerWrapper>();

    private void PrintHandlerAdapter(ReadOnlySpan<char> text)
    {
        _printHandler(text.ToString());
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
        _parser.ExecuteHandler = _executeHandler;
        _parser.EscapeHandler = _escapeHandler;
        _parser.CsiHandler = _csiHandler;
        _parser.DcsHandler = new DcsHandlerWrapper(_dcsHandler);
        _parser.OscHandler = new AuxStringHandlerWrapper(_oscHandler);
        _parser.SosHandler = new AuxStringHandlerWrapper(_sosHandler);
        _parser.PmHandler = new AuxStringHandlerWrapper(_pmHandler);
        _parser.ApcHandler = new AuxStringHandlerWrapper(_apcHandler);
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

    public static TheoryData<string> NormalPrintableTextTestData()
    {
        var data = new TheoryData<string>();

        // All characters in GL area
        for (var code = '\x20'; code <= '\x7F'; ++code)
        {
            data.Add(code.ToString());
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            data.Add(code.ToString());
        }

        // Some consecutive printable sequences
        data.Add("The quick brown fox jumps over the lazy dog.");
        data.Add("¡£¥ª®±¶»¾¿");
        data.Add("ÁÆüßÏÐÑsøþç×m÷Ö");

        return data;
    }

    public static TheoryData<string> UnicodeModePrintableTextTestData()
    {
        var data = new TheoryData<string>();

        // Some Unicode characters
        data.Add("\u0100");
        data.Add("\u5948");
        data.Add("\uFFFF");

        // Some consecutive printable sequences
        data.Add("你好，世界！こんにちは、世界！반갑다 세상아");
        data.Add("Здравствуй, мир!Γεια σου κόσμε!");
        data.Add("😊👍🌟🎉❤️");

        return data;
    }

    [Theory]
    [MemberData(nameof(NormalPrintableTextTestData))]
    public void Parse_LegacyMode_PrintableText_CallPrintHandlerOnce(string input)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        _printHandler.Received().Invoke(input);
    }

    [Theory]
    [MemberData(nameof(NormalPrintableTextTestData))]
    [MemberData(nameof(UnicodeModePrintableTextTestData))]
    public void Parse_UnicodeMode_PrintableText_CallPrintHandlerOnce(string input)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        _printHandler.Received().Invoke(input);
    }

    #endregion

    #region Parse Executable Text Test

    public static TheoryData<string> NormalExecutableTextTestData()
    {
        var data = new TheoryData<string>();

        // All executable C0 controls
        foreach (var code in C0.All.Except([C0.ESC]))
        {
            data.Add(code.ToString());
        }

        // All executable C1 controls
        foreach (var code in C1.All.Except([C1.DCS, C1.SOS, C1.OSC, C1.CSI, C1.ST, C1.PM, C1.APC]))
        {
            data.Add(code.ToString());
        }

        // Consecutive executable sequences
        data.Add("\x07\x0A\x0D\x0E\x08\x0F\x09\x0B");
        data.Add("\x88\x80\x85\x8A\x8E\x94\x99");
        data.Add("\x8F\x12\x0C\x8B\x06\x86\x1C");

        return data;
    }

    [Theory]
    [MemberData(nameof(NormalExecutableTextTestData))]
    public void Parse_LegacyMode_ExecutableText_CallExecuteHandlerCorrectly(string input)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            foreach (var code in input)
            {
                _executeHandler(code);
            }
        });
    }

    [Theory]
    [MemberData(nameof(NormalExecutableTextTestData))]
    public void Parse_UnicodeMode_ExecutableText_CallExecuteHandlerCorrectly(string input)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            foreach (var code in input)
            {
                _executeHandler(code);
            }
        });
    }

    #endregion

    #region Parse Single Function Escape Sequence Test

    public static TheoryData<string, char, string> NormalEscapeSequenceTestData()
    {
        var data = new TheoryData<string, char, string>();
        var validIntermediates = Enumerable.Range('\x30', '\x7E' - '\x30')
            .Except(['\x50', '\x58', '\x5B', '\x5D', '\x5E', '\x5F']);

        // All valid intermediates without collect
        foreach (char code in validIntermediates)
        {
            data.Add(C0.ESC + code.ToString(), code, "");
        }

        // Collects with single valid character
        for (var code = '\x20'; code <= '\x2F'; ++code)
        {
            data.Add(C0.ESC + code.ToString() + "O", 'O', code.ToString());
        }

        // Escape sequence with multiple collect characters
        data.Add(C0.ESC + "+/!(*W", 'W', "+/!(*");

        return data;
    }

    public static TheoryData<string, char, string> LegacyModeEscapeSequenceTestData()
    {
        var data = new TheoryData<string, char, string>();
        var validIntermediates = Enumerable.Range('\x30', '\x7E' - '\x30')
            .Except(['\x50', '\x58', '\x5B', '\x5D', '\x5E', '\x5F']);

        // All valid intermediates without collect
        foreach (char code in validIntermediates)
        {
            data.Add(C0.ESC + ((char)(code + '\x80')).ToString(), code, "");
        }

        // Collects with single valid character
        for (var code = '\x20'; code <= '\x2F'; ++code)
        {
            data.Add(C0.ESC + ((char)(code + '\x80')).ToString() + "s", 's', code.ToString());
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(NormalEscapeSequenceTestData))]
    [MemberData(nameof(LegacyModeEscapeSequenceTestData))]
    public void Parse_LegacyMode_EscapeSequence_CallEscapeHandlerOnce(string input, char expectedCode,
        string expectedCollect)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        _escapeHandler.Received().Invoke(expectedCode, expectedCollect);
    }

    [Theory]
    [MemberData(nameof(NormalEscapeSequenceTestData))]
    public void Parse_UnicodeMode_EscapeSequence_CallEscapeHandlerOnce(string input, char expectedCode,
        string expectedCollect)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        _escapeHandler.Received().Invoke(expectedCode, expectedCollect);
    }

    #endregion

    #region Parse CSI Sequence Test

    public static TheoryData<string, char, string, int[]> NormalCsiSequenceTestData()
    {
        var data = new TheoryData<string, char, string, int[]>();

        // Simple CSI sequences without any collect and parameters
        for (var code = '\x40'; code <= '\x7E'; ++code)
        {
            data.Add(C0.ESC + "[" + code, code, "", [0]);
            data.Add(C1.CSI + code.ToString(), code, "", [0]);
        }

        // CSI sequences with single parameter
        for (var num = 0; num <= 9; ++num)
        {
            data.Add(C0.ESC + "[" + num + "A", 'A', "", [num]);
            data.Add(C1.CSI + num.ToString() + "A", 'A', "", [num]);
        }

        data.Add(C0.ESC + "[65535D", 'D', "", [65535]);
        data.Add(C1.CSI + "824M", 'M', "", [824]);

        // CSI sequences with multiple parameters
        data.Add(C0.ESC + "[185;62S", 'S', "", [185, 62]);
        data.Add(C1.CSI + "595;1345;37Z", 'Z', "", [595, 1345, 37]);

        // CSI sequences with collect
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

        // CSI sequences with parameters and collect
        data.Add(C0.ESC + "[58 @", '@', " ", [58]);
        data.Add(C1.CSI + ">2T", 'T', ">", [2]);
        data.Add(C0.ESC + "[?45;2001l", 'l', "?", [45, 2001]);
        data.Add(C1.CSI + "114;514*%{", '{', "*%", [114, 514]);

        // CSI sequences with empty parameters
        data.Add(C0.ESC + "[;19g", 'g', "", [0, 19]);
        data.Add(C1.CSI + "8;;1~", '~', "", [8, 0, 1]);
        data.Add(C0.ESC + "[3;;,'I", 'I', ",'", [3, 0, 0]);
        data.Add(C1.CSI + ";;;-(q", 'q', "-(", [0, 0, 0, 0]);

        return data;
    }

    public static TheoryData<string, char, string, int[]> LegacyModeCsiSequenceTestData()
    {
        var data = new TheoryData<string, char, string, int[]>();

        // Simple CSI sequences without any collect and parameters
        for (var code = '\x40'; code <= '\x7E'; ++code)
        {
            data.Add(C0.ESC + "[" + (char)(code + '\x80'), code, "", [0]);
            data.Add(C1.CSI + ((char)(code + '\x80')).ToString(), code, "", [0]);
        }

        // CSI sequences with single parameter
        for (var num = 0; num <= 9; ++num)
        {
            data.Add(C0.ESC + "[" + (char)(num + '\xB0') + "\xC1", 'A', "", [num]);
            data.Add(C1.CSI + ((char)(num + '\xB0')).ToString() + "\xC1", 'A', "", [num]);
        }

        // CSI sequences with collect
        for (var collect = '\x20'; collect <= '\x2F'; ++collect)
        {
            data.Add(C0.ESC + "[" + (char)(collect + '\x80') + "F", 'F', collect.ToString(), [0]);
            data.Add(C1.CSI + ((char)(collect + '\x80')).ToString() + "F", 'F', collect.ToString(), [0]);
        }

        for (var collect = '\x3C'; collect <= '\x3F'; ++collect)
        {
            data.Add(C0.ESC + "[" + (char)(collect + '\x80') + "w", 'w', collect.ToString(), [0]);
            data.Add(C1.CSI + ((char)(collect + '\x80')).ToString() + "w", 'w', collect.ToString(), [0]);
        }

        return data;
    }

    [Theory]
    [MemberData(nameof(NormalCsiSequenceTestData))]
    [MemberData(nameof(LegacyModeCsiSequenceTestData))]
    public void Parse_LegacyMode_CsiSequence_CallCsiHandlerCorrectly(string input, char expectedCode,
        string expectedCollect, int[] expectedParam)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        _csiHandler.Received()
            .Invoke(expectedCode, expectedCollect, Arg.Is<int[]>(a => a.SequenceEqual(expectedParam)));
    }

    [Theory]
    [MemberData(nameof(NormalCsiSequenceTestData))]
    public void Parse_UnicodeMode_CsiSequence_CallCsiHandlerCorrectly(string input, char expectedCode,
        string expectedCollect, int[] expectedParam)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        _csiHandler.Received()
            .Invoke(expectedCode, expectedCollect, Arg.Is<int[]>(a => a.SequenceEqual(expectedParam)));
    }

    #endregion

    #region Parse SOS Sequence Test

    public static TheoryData<string, string> NormalSosSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        // All characters in GL area
        for (var code = '\x20'; code <= '\x7F'; ++code)
        {
            data.Add(C0.ESC + "X" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.SOS + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            data.Add(C0.ESC + "X" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.SOS + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // Some consecutive printable sequences
        var contents = new[]
        {
            "The quick brown fox jumps over the lazy dog.",
            "¡£¥ª®±¶»¾¿",
            "ÁÆüßÏÐÑsøþç×m÷Ö",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "X" + content + C0.ESC + "\\", content);
            data.Add(C1.SOS + content + C0.ESC + "\\", content);
        }

        return data;
    }

    public static TheoryData<string, string> LegacyModeSosSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var content = "ÁÆüßÏÐÑsøþç×m÷Ö";
        data.Add(C0.ESC + "\xD8" + content + C0.ESC + "\xDC", content);

        return data;
    }

    public static TheoryData<string, string> UnicodeModeSosSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var contents = new[]
        {
            "\u0100",
            "\u5948",
            "\uFFFF",
            "你好，世界！こんにちは、世界！반갑다 세상아",
            "Здравствуй, мир!Γεια σου κόσμε!",
            "😊😋😮🏢🍔",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "X" + content + C0.ESC + "\\", content);
        }

        return data;
    }

    [Fact]
    public void Parse_SosSequenceWithoutContentAndEndWithC0St_CallSosAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "X" + C0.ESC + "\\");

        Received.InOrder(() =>
        {
            _sosHandler.Start();
            _sosHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Fact]
    public void Parse_SosSequenceWithoutContentAndEndWithC1St_CallSosAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "X" + C0.ESC + C1.ST);

        Received.InOrder(() =>
        {
            _sosHandler.Start();
            _sosHandler.End();
        });
    }

    [Theory]
    [MemberData(nameof(NormalSosSequenceTestData))]
    [MemberData(nameof(LegacyModeSosSequenceTestData))]
    public void Parse_LegacyMode_SosSequence_CallSosAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _sosHandler.Start();
            _sosHandler.Put(expectedContent);
            _sosHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Theory]
    [MemberData(nameof(NormalSosSequenceTestData))]
    [MemberData(nameof(UnicodeModeSosSequenceTestData))]
    public void Parse_UnicodeMode_SosSequence_CallSosAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _sosHandler.Start();
            _sosHandler.Put(expectedContent);
            _sosHandler.End();
            _escapeHandler('\\', "");
        });
    }

    #endregion

    #region Parse OSC Sequence Test

    public static TheoryData<string, string> NormalOscSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        // All characters in GL area
        for (var code = '\x20'; code <= '\x7F'; ++code)
        {
            data.Add(C0.ESC + "]" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.OSC + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            data.Add(C0.ESC + "]" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.OSC + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // Some consecutive printable sequences
        var contents = new[]
        {
            "The quick brown fox jumps over the lazy dog.",
            "¡£¥ª®±¶»¾¿",
            "ÁÆüßÏÐÑsøþç×m÷Ö",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "]" + content + C0.ESC + "\\", content);
            data.Add(C1.OSC + content + C0.ESC + "\\", content);
        }

        return data;
    }

    public static TheoryData<string, string> LegacyModeOscSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var content = "ÁÆüßÏÐÑsøþç×m÷Ö";
        data.Add(C0.ESC + "\xDD" + content + C0.ESC + "\xDC", content);

        return data;
    }

    public static TheoryData<string, string> UnicodeModeOscSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var contents = new[]
        {
            "\u0100",
            "\u5948",
            "\uFFFF",
            "你好，世界！こんにちは、世界！반갑다 세상아",
            "Здравствуй, мир!Γεια σου κόσμε!",
            "😊😋😮🏢🍔",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "]" + content + C0.ESC + "\\", content);
        }

        return data;
    }

    [Fact]
    public void Parse_OscSequenceWithoutContentAndEndWithC0St_CallOscAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "]" + C0.ESC + "\\");

        Received.InOrder(() =>
        {
            _oscHandler.Start();
            _oscHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Fact]
    public void Parse_OscSequenceWithoutContentAndEndWithC1St_CallOscAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "]" + C0.ESC + C1.ST);

        Received.InOrder(() =>
        {
            _oscHandler.Start();
            _oscHandler.End();
        });
    }

    [Theory]
    [MemberData(nameof(NormalOscSequenceTestData))]
    [MemberData(nameof(LegacyModeOscSequenceTestData))]
    public void Parse_LegacyMode_OscSequence_CallOscAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _oscHandler.Start();
            _oscHandler.Put(expectedContent);
            _oscHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Theory]
    [MemberData(nameof(NormalOscSequenceTestData))]
    [MemberData(nameof(UnicodeModeOscSequenceTestData))]
    public void Parse_UnicodeMode_OscSequence_CallOscAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _oscHandler.Start();
            _oscHandler.Put(expectedContent);
            _oscHandler.End();
            _escapeHandler('\\', "");
        });
    }

    #endregion

    #region Parse PM Sequence Test

    public static TheoryData<string, string> NormalPmSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        // All characters in GL area
        for (var code = '\x20'; code <= '\x7F'; ++code)
        {
            data.Add(C0.ESC + "^" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.PM + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            data.Add(C0.ESC + "^" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.PM + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // Some consecutive printable sequences
        var contents = new[]
        {
            "The quick brown fox jumps over the lazy dog.",
            "¡£¥ª®±¶»¾¿",
            "ÁÆüßÏÐÑsøþç×m÷Ö",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "^" + content + C0.ESC + "\\", content);
            data.Add(C1.PM + content + C0.ESC + "\\", content);
        }

        return data;
    }

    public static TheoryData<string, string> LegacyModePmSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var content = "ÁÆüßÏÐÑsøþç×m÷Ö";
        data.Add(C0.ESC + "\xDE" + content + C0.ESC + "\xDC", content);

        return data;
    }

    public static TheoryData<string, string> UnicodeModePmSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var contents = new[]
        {
            "\u0100",
            "\u5948",
            "\uFFFF",
            "你好，世界！こんにちは、世界！반갑다 세상아",
            "Здравствуй, мир!Γεια σου κόσμε!",
            "😊😋😮🏢🍔",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "^" + content + C0.ESC + "\\", content);
        }

        return data;
    }

    [Fact]
    public void Parse_PmSequenceWithoutContentAndEndWithC0St_CallPmAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "^" + C0.ESC + "\\");

        Received.InOrder(() =>
        {
            _pmHandler.Start();
            _pmHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Fact]
    public void Parse_PmSequenceWithoutContentAndEndWithC1St_CallPmAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "^" + C0.ESC + C1.ST);

        Received.InOrder(() =>
        {
            _pmHandler.Start();
            _pmHandler.End();
        });
    }

    [Theory]
    [MemberData(nameof(NormalPmSequenceTestData))]
    [MemberData(nameof(LegacyModePmSequenceTestData))]
    public void Parse_LegacyMode_PmSequence_CallPmAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _pmHandler.Start();
            _pmHandler.Put(expectedContent);
            _pmHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Theory]
    [MemberData(nameof(NormalPmSequenceTestData))]
    [MemberData(nameof(UnicodeModePmSequenceTestData))]
    public void Parse_UnicodeMode_PmSequence_CallPmAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _pmHandler.Start();
            _pmHandler.Put(expectedContent);
            _pmHandler.End();
            _escapeHandler('\\', "");
        });
    }

    #endregion

    #region Parse APC Sequence Test

    public static TheoryData<string, string> NormalApcSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        // All characters in GL area
        for (var code = '\x20'; code <= '\x7F'; ++code)
        {
            data.Add(C0.ESC + "_" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.APC + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // All characters in GR area
        for (var code = '\xA0'; code <= '\xFF'; ++code)
        {
            data.Add(C0.ESC + "_" + code + C0.ESC + "\\", code.ToString());
            data.Add(C1.APC + code.ToString() + C0.ESC + "\\", code.ToString());
        }

        // Some consecutive printable sequences
        var contents = new[]
        {
            "The quick brown fox jumps over the lazy dog.",
            "¡£¥ª®±¶»¾¿",
            "ÁÆüßÏÐÑsøþç×m÷Ö",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "_" + content + C0.ESC + "\\", content);
            data.Add(C1.APC + content + C0.ESC + "\\", content);
        }

        return data;
    }

    public static TheoryData<string, string> LegacyModeApcSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var content = "ÁÆüßÏÐÑsøþç×m÷Ö";
        data.Add(C0.ESC + "\xDF" + content + C0.ESC + "\xDC", content);

        return data;
    }

    public static TheoryData<string, string> UnicodeModeApcSequenceTestData()
    {
        var data = new TheoryData<string, string>();

        var contents = new[]
        {
            "\u0100",
            "\u5948",
            "\uFFFF",
            "你好，世界！こんにちは、世界！반갑다 세상아",
            "Здравствуй, мир!Γεια σου κόσμε!",
            "😊😋😮🏢🍔",
        };
        foreach (var content in contents)
        {
            data.Add(C0.ESC + "_" + content + C0.ESC + "\\", content);
        }

        return data;
    }

    [Fact]
    public void Parse_ApcSequenceWithoutContentAndEndWithC0St_CallApcAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "_" + C0.ESC + "\\");

        Received.InOrder(() =>
        {
            _apcHandler.Start();
            _apcHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Fact]
    public void Parse_ApcSequenceWithoutContentAndEndWithC1St_CallApcAndEscapeHandlerCorrectly()
    {
        _parser.Parse(C0.ESC + "_" + C0.ESC + C1.ST);

        Received.InOrder(() =>
        {
            _apcHandler.Start();
            _apcHandler.End();
        });
    }

    [Theory]
    [MemberData(nameof(NormalApcSequenceTestData))]
    [MemberData(nameof(LegacyModeApcSequenceTestData))]
    public void Parse_LegacyMode_ApcSequence_CallApcAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = true;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _apcHandler.Start();
            _apcHandler.Put(expectedContent);
            _apcHandler.End();
            _escapeHandler('\\', "");
        });
    }

    [Theory]
    [MemberData(nameof(NormalApcSequenceTestData))]
    [MemberData(nameof(UnicodeModeApcSequenceTestData))]
    public void Parse_UnicodeMode_ApcSequence_CallApcAndEscapeHandlerCorrectly(string input, string expectedContent)
    {
        _parser.LegacyMode = false;
        _parser.Parse(input);

        Received.InOrder(() =>
        {
            _apcHandler.Start();
            _apcHandler.Put(expectedContent);
            _apcHandler.End();
            _escapeHandler('\\', "");
        });
    }

    #endregion
}