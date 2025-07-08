using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace AvaTerm.Parser
{
    internal class EscapeSequenceParser
    {
        private enum Action
        {
            Invalid = 0,
            Ignore = 1,
            Print,
            Execute,
            Clear,
            Collect,
            Param,
            EscDispatch,
            CsiDispatch,
            Hook,
            Put,
            Unhook,
            AuxStringStart,
            AuxStringPut,
            AuxStringEnd,
        }

        private enum State
        {
            Invalid = 0,
            Ground = 1,
            Escape,
            EscapeIntermediate,
            CsiEntry,
            CsiIntermediate,
            CsiIgnore,
            CsiParam,
            DcsEntry,
            DcsIntermediate,
            DcsIgnore,
            DcsParam,
            DcsPassthrough,
            AuxString,
        }

        private class TransitionTable
        {
            private readonly byte[] _table;

            private static uint EncodeIndex(byte code, State state)
            {
                return ((uint)state << 8) | code;
            }

            private static byte EncodeValue(Action action, State state)
            {
                return (byte)(((uint)action << 4) | (uint)state);
            }

            private static (Action transitionAction, State nextState) DecodeValue(byte value)
            {
                var actionValue = (value & 0xF0) >> 4;
                var stateValue = value & 0x0F;

                Debug.Assert(Enum.IsDefined(typeof(Action), actionValue));
                Debug.Assert(Enum.IsDefined(typeof(State), stateValue));

                return ((Action)actionValue, (State)stateValue);
            }

            public TransitionTable()
            {
                Debug.Assert(ActionEnumSize <= 4);
                Debug.Assert(StateEnumSize <= 4);

                var length = (uint)Math.Pow(2, StateEnumSize + 8);
                _table = new byte[length];
            }

            public void Add(byte code, State state, State nextState, Action action)
            {
                _table[EncodeIndex(code, state)] = EncodeValue(action, nextState);
            }

            public void Add(byte code, State[] states, State nextState, Action action)
            {
                foreach (var state in states)
                {
                    Add(code, state, nextState, action);
                }
            }

            public void Add(byte[] codes, State state, State nextState, Action action)
            {
                foreach (var code in codes)
                {
                    Add(code, state, nextState, action);
                }
            }

            public void Add(byte[] codes, State[] states, State nextState, Action action)
            {
                foreach (var state in states)
                {
                    Add(codes, state, nextState, action);
                }
            }

            public (Action transitionAction, State nextState) Transition(byte code, State state)
            {
                return DecodeValue(_table[EncodeIndex(code, state)]);
            }
        }

        private class EventTable
        {
            private readonly byte[] _table;

            private static uint EncodeIndex(State state, State nextState)
            {
                return ((uint)state << 4) | (uint)nextState;
            }

            private static byte EncodeValue(Action entryAction, Action exitAction)
            {
                return (byte)(((uint)entryAction << 4) | (uint)exitAction);
            }

            private static (Action entryAction, Action exitAction) DecodeValue(byte value)
            {
                var entryAction = (value & 0xF0) >> 4;
                var exitAction = value & 0x0F;

                Debug.Assert(Enum.IsDefined(typeof(Action), entryAction));
                Debug.Assert(Enum.IsDefined(typeof(Action), exitAction));

                return ((Action)entryAction, (Action)exitAction);
            }

            public EventTable()
            {
                Debug.Assert(ActionEnumSize <= 4);
                Debug.Assert(StateEnumSize <= 4);

                var length = (uint)Math.Pow(2, 8);
                _table = new byte[length];
            }

            public void AddEntry(State state, Action action)
            {
                var otherStates = AllStates.Except(new[] { state }).ToArray();

                foreach (var otherState in otherStates)
                {
                    var index = EncodeIndex(otherState, state);
                    var (_, exitAction) = DecodeValue(_table[index]);
                    _table[index] = EncodeValue(action, exitAction);
                }
            }

            public void AddExit(State state, Action action)
            {
                var otherStates = AllStates.Except(new[] { state }).ToArray();

                foreach (var otherState in otherStates)
                {
                    var index = EncodeIndex(state, otherState);
                    var (entryAction, _) = DecodeValue(_table[index]);
                    _table[index] = EncodeValue(entryAction, action);
                }
            }

            public (Action entryAction, Action exitAction) Emit(State state, State nextState)
            {
                return DecodeValue(_table[EncodeIndex(state, nextState)]);
            }
        }

        public interface IDcsHandler
        {
            void Hook(char code, string collect, int[] parameters);
            void Put(ReadOnlySpan<char> text);
            void Unhook();
        }

        public interface IAuxStringHandler
        {
            void Start();
            void Put(ReadOnlySpan<char> text);
            void End();
        }

        public delegate void PrintHandlerAction(ReadOnlySpan<char> text);

        private const byte NonAsciiPrintable = 0xA0;
        private static readonly uint ActionEnumSize = GetEnumMinSize(typeof(Action));
        private static readonly uint StateEnumSize = GetEnumMinSize(typeof(State));
        private static readonly State[] AllStates = Enum.GetValues(typeof(State)).Cast<State>().ToArray();
        private const State InitState = State.Ground;

        private readonly TransitionTable _transitionTable = BuildVt500TransitionTable();
        private readonly EventTable _eventTable = BuildVt500EventTable();

        // Parser state variables
        private State _state = InitState;
        private string _collect = "";
        private List<int> _param = new List<int> { 0 };
        private IAuxStringHandler _activeAuxStringHandler;

        public bool LegacyMode { get; set; }
        public PrintHandlerAction PrintHandler { set; get; }
        public Action<char> ExecuteHandler { set; get; }
        public Action<char, string> EscapeHandler { set; get; }
        public Action<char, string, int[]> CsiHandler { set; get; }
        public IDcsHandler DcsHandler { set; get; }
        public IAuxStringHandler OscHandler { set; get; }
        public IAuxStringHandler SosHandler { set; get; }
        public IAuxStringHandler PmHandler { set; get; }
        public IAuxStringHandler ApcHandler { set; get; }

        private static uint GetEnumMinSize(Type enumType)
        {
            var maxValue = Enum.GetValues(enumType).Cast<uint>().Max();
            return (uint)Math.Ceiling(Math.Log(maxValue, 2));
        }

        private static byte[] CodeRange(byte first, byte last)
        {
            Debug.Assert(first <= last);

            var rst = new byte[last - first + 1];

            for (var i = first; i <= last; ++i)
            {
                rst[i - first] = i;
            }

            return rst;
        }

        private static TransitionTable BuildVt500TransitionTable()
        {
            var table = new TransitionTable();

            // Global anywhere transitions
            // ---------------------------
            // anywhere -> ground: 0x18, 0x1A, 0x80-0x8F, 0x91-0x97, 0x99, 0x9A / execute
            table.Add(new byte[] { 0x18, 0x1A, 0x99, 0x9A }, AllStates, State.Ground, Action.Execute);
            table.Add(CodeRange(0x80, 0x8F), AllStates, State.Ground, Action.Execute);
            table.Add(CodeRange(0x91, 0x97), AllStates, State.Ground, Action.Execute);
            // anywhere -> ground: 0x9C / ignore
            table.Add(0x9C, AllStates, State.Ground, Action.Ignore);
            // anywhere -> ground: 0xA0 / print (fallback)
            table.Add(NonAsciiPrintable, AllStates, State.Ground, Action.Print);
            // anywhere -> escape: 0x1B / ignore
            table.Add(0x1B, AllStates, State.Escape, Action.Ignore);
            // anywhere -> dcs entry: 0x90 / ignore
            table.Add(0x90, AllStates, State.DcsEntry, Action.Ignore);
            // anywhere -> aux string: 0x98, 0x9D-0x9F / ignore
            table.Add(0x98, AllStates, State.AuxString, Action.Ignore);
            table.Add(CodeRange(0x9D, 0x9F), AllStates, State.AuxString, Action.Ignore);
            // anywhere -> csi entry: 0x9B / ignore
            table.Add(0x9B, AllStates, State.CsiEntry, Action.Ignore);

            // Global return to ground transitions
            // -----------------------------------
            // csi ignore -> ground: 0x40-0x7E / ignore
            table.Add(CodeRange(0x40, 0x7E), State.CsiIgnore, State.Ground, Action.Ignore);
            // aux string -> ground: 0x9C / ignore
            table.Add(0x9C, State.AuxString, State.Ground, Action.Ignore);
            // dcs ignore -> ground: 0x9C / ignore
            table.Add(0x9C, State.DcsIgnore, State.Ground, Action.Ignore);
            // dcs passthrough -> ground: 0x9C / ignore
            table.Add(0x9C, State.DcsPassthrough, State.Ground, Action.Ignore);

            // To ground transitions
            // ---------------------
            // escape intermediate -> ground: 0x30-0x7E / esc_dispatch
            table.Add(CodeRange(0x30, 0x7E), State.EscapeIntermediate, State.Ground, Action.EscDispatch);
            // escape -> ground: 0x30-0x4F, 0x51-0x57, 0x59, 0x5A, 0x5C, 0x60-0x7E / esc_dispatch
            table.Add(new byte[] { 0x59, 0x5A, 0x5C }, State.Escape, State.Ground, Action.EscDispatch);
            table.Add(CodeRange(0x30, 0x4F), State.Escape, State.Ground, Action.EscDispatch);
            table.Add(CodeRange(0x51, 0x57), State.Escape, State.Ground, Action.EscDispatch);
            table.Add(CodeRange(0x60, 0x7E), State.Escape, State.Ground, Action.EscDispatch);
            // csi param -> ground: 0x40-0x7E / csi_dispatch
            table.Add(CodeRange(0x40, 0x7E), State.CsiParam, State.Ground, Action.CsiDispatch);
            // csi intermediate -> ground: 0x40-0x7E / csi_dispatch
            table.Add(CodeRange(0x40, 0x7E), State.CsiIntermediate, State.Ground, Action.CsiDispatch);
            // csi entry -> ground: 0x40-0x7E / csi_dispatch
            table.Add(CodeRange(0x40, 0x7E), State.CsiEntry, State.Ground, Action.CsiDispatch);

            // To escape intermediate transitions
            // ----------------------------------
            // escape -> escape intermediate: 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.Escape, State.EscapeIntermediate, Action.Collect);

            // To dcs entry transitions
            // ------------------------
            // escape -> dcs entry: 0x50 / ignore
            table.Add(0x50, State.Escape, State.DcsEntry, Action.Ignore);

            // To dcs intermediate transitions
            // -------------------------------
            // dcs entry -> dcs intermediate: 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.DcsEntry, State.DcsIntermediate, Action.Collect);
            // dcs param -> dcs intermediate: 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.DcsParam, State.DcsIntermediate, Action.Collect);

            // To dcs ignore transitions
            // -------------------------
            // dcs entry -> dcs ignore: 0x3A / ignore
            table.Add(0x3A, State.DcsEntry, State.DcsIgnore, Action.Ignore);
            // dcs intermediate -> dcs ignore: 0x30-0x3F / ignore
            table.Add(CodeRange(0x30, 0x3F), State.DcsIntermediate, State.DcsIgnore, Action.Ignore);
            // dcs param -> dcs ignore: 0x3A, 0x3C-0x3F / ignore
            table.Add(0x3A, State.DcsParam, State.DcsIgnore, Action.Ignore);
            table.Add(CodeRange(0x3C, 0x3F), State.DcsParam, State.DcsIgnore, Action.Ignore);

            // To dcs param transitions
            // ------------------------
            // dcs entry -> dcs param: 0x30-0x39, 0x3B / param
            table.Add(CodeRange(0x30, 0x39), State.DcsEntry, State.DcsParam, Action.Param);
            table.Add(0x3B, State.DcsEntry, State.DcsParam, Action.Param);
            // dcs entry -> dcs param: 0x3C-0x3F / collect
            table.Add(CodeRange(0x3C, 0x3F), State.DcsEntry, State.DcsParam, Action.Collect);

            // To dcs passthrough transitions
            // ------------------------------
            // dcs entry -> dcs passthrough: 0x40-0x7E / ignore
            table.Add(CodeRange(0x40, 0x7E), State.DcsEntry, State.DcsPassthrough, Action.Ignore);
            // dcs intermediate -> dcs passthrough: 0x40-0x7E / ignore
            table.Add(CodeRange(0x40, 0x7E), State.DcsIntermediate, State.DcsPassthrough, Action.Ignore);
            // dcs param -> dcs passthrough: 0x40-0x7E / ignore
            table.Add(CodeRange(0x40, 0x7E), State.DcsParam, State.DcsPassthrough, Action.Ignore);

            // To aux string transitions
            // -------------------------
            // escape -> aux string: 0x58, 0x5D-0x5F / ignore
            table.Add(0x58, State.Escape, State.AuxString, Action.Ignore);
            table.Add(CodeRange(0x5D, 0x5F), State.Escape, State.AuxString, Action.Ignore);

            // To csi entry transitions
            // ------------------------
            // escape -> csi entry: 0x5B / ignore
            table.Add(0x5B, State.Escape, State.CsiEntry, Action.Ignore);

            // To csi intermediate transitions
            // -------------------------------
            // csi entry -> csi intermediate: 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.CsiEntry, State.CsiIntermediate, Action.Collect);
            // csi param -> csi intermediate: 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.CsiParam, State.CsiIntermediate, Action.Collect);

            // To csi ignore transitions
            // -------------------------
            // csi entry -> csi ignore: 0x3A / ignore
            table.Add(0x3A, State.CsiEntry, State.CsiIgnore, Action.Ignore);
            // csi intermediate -> csi ignore: 0x30-0x3F / ignore
            table.Add(CodeRange(0x30, 0x3F), State.CsiIntermediate, State.CsiIgnore, Action.Ignore);
            // csi param -> csi ignore: 0x3A, 0x3C-0x3F / ignore
            table.Add(0x3A, State.CsiParam, State.CsiIgnore, Action.Ignore);
            table.Add(CodeRange(0x3C, 0x3F), State.CsiParam, State.CsiIgnore, Action.Ignore);

            // To csi param transitions
            // ------------------------
            // csi entry -> csi param: 0x30-0x39, 0x3B / param
            table.Add(CodeRange(0x30, 0x39), State.CsiEntry, State.CsiParam, Action.Param);
            table.Add(0x3B, State.CsiEntry, State.CsiParam, Action.Param);
            // csi entry -> csi param: 0x3C-0x3F / collect
            table.Add(CodeRange(0x3C, 0x3F), State.CsiEntry, State.CsiParam, Action.Collect);

            // Ground spin transitions
            // -----------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.Ground, State.Ground, Action.Execute);
            table.Add(0x19, State.Ground, State.Ground, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.Ground, State.Ground, Action.Execute);
            // event 0x20-0x7F, 0xA0 / print
            table.Add(CodeRange(0x20, 0x7F), State.Ground, State.Ground, Action.Print);
            table.Add(NonAsciiPrintable, State.Ground, State.Ground, Action.Print);

            // Escape intermediate spin transitions
            // ------------------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.EscapeIntermediate, State.EscapeIntermediate, Action.Execute);
            table.Add(0x19, State.EscapeIntermediate, State.EscapeIntermediate, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.EscapeIntermediate, State.EscapeIntermediate, Action.Execute);
            // event 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.EscapeIntermediate, State.EscapeIntermediate, Action.Collect);
            // event 0x7F / ignore
            table.Add(0x7F, State.EscapeIntermediate, State.EscapeIntermediate, Action.Ignore);

            // Escape spin transitions
            // -----------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.Escape, State.Escape, Action.Execute);
            table.Add(0x19, State.Escape, State.Escape, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.Escape, State.Escape, Action.Execute);
            // event 0x7F / ignore
            table.Add(0x7F, State.Escape, State.Escape, Action.Ignore);

            // Dcs entry spin transitions
            // --------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F, 0x7F / ignore
            table.Add(CodeRange(0x00, 0x17), State.DcsEntry, State.DcsEntry, Action.Ignore);
            table.Add(new byte[] { 0x19, 0x7F }, State.DcsEntry, State.DcsEntry, Action.Ignore);
            table.Add(CodeRange(0x1C, 0x1F), State.DcsEntry, State.DcsEntry, Action.Ignore);

            // Dcs intermediate spin transitions
            // ---------------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F, 0x7F / ignore
            table.Add(CodeRange(0x00, 0x17), State.DcsIntermediate, State.DcsIntermediate, Action.Ignore);
            table.Add(new byte[] { 0x19, 0x7F }, State.DcsIntermediate, State.DcsIntermediate, Action.Ignore);
            table.Add(CodeRange(0x1C, 0x1F), State.DcsIntermediate, State.DcsIntermediate, Action.Ignore);
            // event 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.DcsIntermediate, State.DcsIntermediate, Action.Collect);

            // Dcs ignore spin transitions
            // ---------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F, 0x20-0x7F, 0xA0 / ignore
            table.Add(CodeRange(0x00, 0x17), State.DcsIgnore, State.DcsIgnore, Action.Ignore);
            table.Add(0x19, State.DcsIgnore, State.DcsIgnore, Action.Ignore);
            table.Add(CodeRange(0x1C, 0x1F), State.DcsIgnore, State.DcsIgnore, Action.Ignore);
            table.Add(CodeRange(0x20, 0x7F), State.DcsIgnore, State.DcsIgnore, Action.Ignore);
            table.Add(NonAsciiPrintable, State.DcsIgnore, State.DcsIgnore, Action.Ignore);

            // Dcs param spin transitions
            // --------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F, 0x7F / ignore
            table.Add(CodeRange(0x00, 0x17), State.DcsParam, State.DcsParam, Action.Ignore);
            table.Add(new byte[] { 0x19, 0x7F }, State.DcsParam, State.DcsParam, Action.Ignore);
            table.Add(CodeRange(0x1C, 0x1F), State.DcsParam, State.DcsParam, Action.Ignore);
            // event 0x30-0x39, 0x3B / param
            table.Add(CodeRange(0x30, 0x39), State.DcsParam, State.DcsParam, Action.Param);
            table.Add(0x3B, State.DcsParam, State.DcsParam, Action.Param);

            // Dcs passthrough spin transitions
            // --------------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F, 0x20-0x7E, 0xA0 / put
            table.Add(CodeRange(0x00, 0x17), State.DcsPassthrough, State.DcsPassthrough, Action.Put);
            table.Add(new byte[] { 0x19, NonAsciiPrintable }, State.DcsPassthrough, State.DcsPassthrough,
                Action.Put);
            table.Add(CodeRange(0x1C, 0x1F), State.DcsPassthrough, State.DcsPassthrough, Action.Put);
            table.Add(CodeRange(0x20, 0x7E), State.DcsPassthrough, State.DcsPassthrough, Action.Put);
            // event 0x7F / ignore
            table.Add(0x7F, State.DcsPassthrough, State.DcsPassthrough, Action.Ignore);

            // Aux string spin transitions
            // ---------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / ignore
            table.Add(CodeRange(0x00, 0x17), State.AuxString, State.AuxString, Action.Ignore);
            table.Add(0x19, State.AuxString, State.AuxString, Action.Ignore);
            table.Add(CodeRange(0x1C, 0x1F), State.AuxString, State.AuxString, Action.Ignore);
            // event 0x20-0x7F, 0xA0 / aux string put
            table.Add(CodeRange(0x20, 0x7F), State.AuxString, State.AuxString, Action.AuxStringPut);
            table.Add(NonAsciiPrintable, State.AuxString, State.AuxString, Action.AuxStringPut);

            // Csi entry spin transitions
            // --------------------------
            // event 0x00-0x17 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.CsiEntry, State.CsiEntry, Action.Execute);
            table.Add(0x19, State.CsiEntry, State.CsiEntry, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.CsiEntry, State.CsiEntry, Action.Execute);
            // event 0x7F / ignore
            table.Add(0x7F, State.CsiEntry, State.CsiEntry, Action.Ignore);

            // Csi intermediate spin transitions
            // ---------------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.CsiIntermediate, State.CsiIntermediate, Action.Execute);
            table.Add(0x19, State.CsiIntermediate, State.CsiIntermediate, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.CsiIntermediate, State.CsiIntermediate, Action.Execute);
            // event 0x20-0x2F / collect
            table.Add(CodeRange(0x20, 0x2F), State.CsiIntermediate, State.CsiIntermediate, Action.Collect);
            // event 0x7F / ignore
            table.Add(0x7F, State.CsiIntermediate, State.CsiIntermediate, Action.Ignore);

            // Csi ignore spin transitions
            // ---------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.CsiIgnore, State.CsiIgnore, Action.Execute);
            table.Add(0x19, State.CsiIgnore, State.CsiIgnore, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.CsiIgnore, State.CsiIgnore, Action.Execute);
            // event 0x20-0x3F, 0x7F, 0xA0 / ignore
            table.Add(CodeRange(0x20, 0x3F), State.CsiIgnore, State.CsiIgnore, Action.Ignore);
            table.Add(new byte[] { 0x7F, NonAsciiPrintable }, State.CsiIgnore, State.CsiIgnore, Action.Ignore);

            // Csi param spin transitions
            // --------------------------
            // event 0x00-0x17, 0x19, 0x1C-0x1F / execute
            table.Add(CodeRange(0x00, 0x17), State.CsiParam, State.CsiParam, Action.Execute);
            table.Add(0x19, State.CsiParam, State.CsiParam, Action.Execute);
            table.Add(CodeRange(0x1C, 0x1F), State.CsiParam, State.CsiParam, Action.Execute);
            // event 0x30-0x39, 0x3B / param
            table.Add(CodeRange(0x30, 0x39), State.CsiParam, State.CsiParam, Action.Param);
            table.Add(0x3B, State.CsiParam, State.CsiParam, Action.Param);
            // event 0x7F / ignore
            table.Add(0x7F, State.CsiParam, State.CsiParam, Action.Ignore);

            return table;
        }

        private static EventTable BuildVt500EventTable()
        {
            var table = new EventTable();

            // Escape events
            // -------------
            // entry / clear
            table.AddEntry(State.Escape, Action.Clear);

            // Dcs entry events
            // ----------------
            // entry / clear
            table.AddEntry(State.DcsEntry, Action.Clear);

            // Dcs passthrough events
            // ----------------------
            // entry / hook
            table.AddEntry(State.DcsPassthrough, Action.Hook);
            // exit / unhook
            table.AddExit(State.DcsPassthrough, Action.Unhook);

            // Aux string events
            // -----------------
            // entry / aux string start
            table.AddEntry(State.AuxString, Action.AuxStringStart);
            // exit / aux string end
            table.AddExit(State.AuxString, Action.AuxStringEnd);

            // Csi entry events
            // ----------------
            // entry / clear
            table.AddEntry(State.CsiEntry, Action.Clear);

            return table;
        }

        private byte MapCode(char code)
        {
            if (LegacyMode)
            {
                if (code > 0x00FF)
                {
                    // Handle invalid codes
                    return 0x00;
                }
                else if (code >= 0x00A0)
                {
                    // Map all codes in GR area to GL area
                    return (byte)(code & 0x007F);
                }
                else
                {
                    return (byte)(code & 0x00FF);
                }
            }
            else
            {
                if (code >= 0x00A0)
                {
                    // Map all Unicode codes greater than 0xA0 to 0xA0
                    return 0xA0;
                }
                else
                {
                    return (byte)(code & 0x00FF);
                }
            }
        }

        public void Reset()
        {
            _state = InitState;
            _collect = "";
            _param = new List<int> { 0 };
            _activeAuxStringHandler = null;
            LegacyMode = false;
        }

        public void Parse(ReadOnlySpan<char> data)
        {
            for (var i = 0; i < data.Length; ++i)
            {
                var code = MapCode(data[i]);

                // Transition states and emit events
                var (transitionAction, nextState) = _transitionTable.Transition(code, _state);
                var (entryAction, exitAction) = _eventTable.Emit(_state, nextState);

                Debug.Assert(nextState != State.Invalid, "Invalid state");
                Debug.Assert(transitionAction != Action.Invalid, "Invalid transition action");

                // Handle exit event
                switch (exitAction)
                {
                    case Action.Unhook:
                        DcsHandler?.Unhook();
                        break;

                    case Action.AuxStringEnd:
                        _activeAuxStringHandler?.End();
                        _activeAuxStringHandler = null;
                        break;

                    case Action.Invalid:
                        break;

                    default:
                        Debug.Assert(false, "Unexpected action");
                        break;
                }

                // Handle transition event
                switch (transitionAction)
                {
                    case Action.Execute:
                        ExecuteHandler?.Invoke((char)code);
                        break;

                    case Action.Print:
                        // Read ahead, and find the consecutive printable sequence to reduce calls
                        for (var j = i + 1;; ++j)
                        {
                            var nextCode = (j < data.Length) ? MapCode(data[j]) : (byte)0x00;

                            if (j >= data.Length || !(nextCode >= 0x20 && nextCode <= 0x7F || nextCode == 0xA0))
                            {
                                PrintHandler?.Invoke(data.Slice(i, j - i));
                                i = j - 1;
                                break;
                            }
                        }

                        break;

                    case Action.Collect:
                        _collect += (char)code;
                        break;

                    case Action.EscDispatch:
                        EscapeHandler?.Invoke((char)code, _collect);
                        break;

                    case Action.Param:
                        // Param is a number list separated by semicolon
                        if (code == 0x3B)
                        {
                            _param.Add(0);
                        }
                        else
                        {
                            var index = _param.Count - 1;
                            _param[index] = _param[index] * 10 + code - 0x30;
                        }

                        break;

                    case Action.CsiDispatch:
                        CsiHandler?.Invoke((char)code, _collect, _param.ToArray());
                        break;

                    case Action.Put:
                        // Read ahead, and find the consecutive valid sequence to reduce calls
                        for (var j = i + 1;; ++j)
                        {
                            var nextCode = (j < data.Length) ? MapCode(data[j]) : (byte)0x00;

                            if (j >= data.Length || !(nextCode <= 0x17 || nextCode == 0x19 ||
                                                      nextCode >= 0x1C && nextCode <= 0x1F ||
                                                      nextCode >= 0x20 && nextCode <= 0x7E || nextCode == 0xA0))
                            {
                                DcsHandler?.Put(data.Slice(i, j - i));
                                i = j - 1;
                                break;
                            }
                        }

                        break;

                    case Action.AuxStringPut:
                        // Read ahead, and find the consecutive valid sequence to reduce calls
                        for (var j = i + 1;; ++j)
                        {
                            var nextCode = (j < data.Length) ? MapCode(data[j]) : (byte)0x00;

                            if (j >= data.Length || !(nextCode >= 0x20 && nextCode <= 0x7F || nextCode == 0xA0))
                            {
                                _activeAuxStringHandler?.Put(data.Slice(i, j - i));
                                i = j - 1;
                                break;
                            }
                        }

                        break;

                    case Action.Ignore:
                        break;

                    default:
                        Debug.Assert(false, "Unexpected action");
                        break;
                }

                // Handle entry event
                switch (entryAction)
                {
                    case Action.Clear:
                        _collect = "";
                        _activeAuxStringHandler = null;
                        _param.Clear();
                        _param.Add(0);
                        break;

                    case Action.Hook:
                        DcsHandler?.Hook((char)code, _collect, _param.ToArray());
                        break;

                    case Action.AuxStringStart:
                        switch (code)
                        {
                            case 0x58:
                            case 0x98:
                                _activeAuxStringHandler = SosHandler;
                                break;

                            case 0x5D:
                            case 0x9D:
                                _activeAuxStringHandler = OscHandler;
                                break;

                            case 0x5E:
                            case 0x9E:
                                _activeAuxStringHandler = PmHandler;
                                break;

                            case 0x5F:
                            case 0x9F:
                                _activeAuxStringHandler = ApcHandler;
                                break;

                            default:
                                Debug.Assert(false, "Unexpected character");
                                break;
                        }

                        _activeAuxStringHandler?.Start();
                        break;

                    case Action.Invalid:
                        break;

                    default:
                        Debug.Assert(false, "Unexpected action");
                        break;
                }

                // Update state
                _state = nextState;
            }
        }
    }
}