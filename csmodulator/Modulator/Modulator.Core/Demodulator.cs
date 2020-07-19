using System;

namespace Modulator.Core
{
    public class Demodulator
    {
        public static IEntry Demodulate(string input)
        {
            var ptr = 0;
            return DemodulateInternal(input, ref ptr);
        }

        private static IEntry DemodulateInternal(string input, ref int ptr)
        {
            var prefix = input.Substring(ptr, 2);
            ptr += 2;
            switch (prefix)
            {
                case "00":
                    return new Null();
                case "11":
                    var first = DemodulateInternal(input, ref ptr);
                    var second = DemodulateInternal(input, ref ptr);
                    return new Pair(first, second);
                case "01":
                    return new Number(DemodulateNumber(input, ref ptr));
                case "10":
                    return new Number(-DemodulateNumber(input, ref ptr));
                default:
                    throw new Exception("wtf");
            }
        }

        private static int DemodulateNumber(string input, ref int ptr)
        {
            if (input[ptr] == '0')
            {
                ptr++;
                return 0;
            }

            var bound = 1;
            while (input[ptr] == '1')
            {
                bound *= 16;
                ptr++;
            }

            bound /= 2;
            var result = 0;
            ptr++;
            while (bound > 0)
            {
                if (input[ptr] == '1')
                    result += bound;
                ptr++;
                bound /= 2;
            }

            return result;
        }
    }
}
