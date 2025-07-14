using System.Linq;

namespace AvaTerm.Data
{
    internal static class C0
    {
        private const char Start = NUL;
        private const char End = US;

        public const char NUL = '\u0000';
        public const char SOH = '\u0001';
        public const char STX = '\u0002';
        public const char ETX = '\u0003';
        public const char EOT = '\u0004';
        public const char ENQ = '\u0005';
        public const char ACK = '\u0006';
        public const char BEL = '\u0007';
        public const char BS = '\u0008';
        public const char HT = '\u0009';
        public const char LF = '\u000A';
        public const char VT = '\u000B';
        public const char FF = '\u000C';
        public const char CR = '\u000D';
        public const char SO = '\u000E';
        public const char SI = '\u000F';
        public const char DLE = '\u0010';
        public const char DC1 = '\u0011';
        public const char DC2 = '\u0012';
        public const char DC3 = '\u0013';
        public const char DC4 = '\u0014';
        public const char NAK = '\u0015';
        public const char SYN = '\u0016';
        public const char ETB = '\u0017';
        public const char CAN = '\u0018';
        public const char EM = '\u0019';
        public const char SUB = '\u001A';
        public const char ESC = '\u001B';
        public const char FS = '\u001C';
        public const char GS = '\u001D';
        public const char RS = '\u001E';
        public const char US = '\u001F';

        public static readonly char[] All = Enumerable.Range(Start, End - Start + 1).Select(c => (char)c).ToArray();
    }
}