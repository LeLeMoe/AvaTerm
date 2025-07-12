using System.Linq;

namespace AvaTerm.Data
{
    internal class C1Controls
    {
        private const char Start = PAD;
        private const char End = APC;

        public const char PAD = '\u0080';
        public const char HOP = '\u0081';
        public const char BPH = '\u0082';
        public const char NBH = '\u0083';
        public const char IND = '\u0084';
        public const char NEL = '\u0085';
        public const char SSA = '\u0086';
        public const char ESA = '\u0087';
        public const char HTS = '\u0088';
        public const char HTJ = '\u0089';
        public const char VTS = '\u008A';
        public const char PLD = '\u008B';
        public const char PLU = '\u008C';
        public const char RI = '\u008D';
        public const char SS2 = '\u008E';
        public const char SS3 = '\u008F';
        public const char DCS = '\u0090';
        public const char PU1 = '\u0091';
        public const char PU2 = '\u0092';
        public const char STS = '\u0093';
        public const char CCH = '\u0094';
        public const char MW = '\u0095';
        public const char SPA = '\u0096';
        public const char EPA = '\u0097';
        public const char SOS = '\u0098';
        public const char SGCI = '\u0099';
        public const char SCI = '\u009A';
        public const char CSI = '\u009B';
        public const char ST = '\u009C';
        public const char OSC = '\u009D';
        public const char PM = '\u009E';
        public const char APC = '\u009F';

        public static readonly char[] All = Enumerable.Range(Start, End - Start + 1).Select(c => (char)c).ToArray();
    }
}