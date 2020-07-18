using System;
using System.Collections.Generic;
using System.Text;

namespace Modulator.Core
{
    public class Parser
    {
        public static IEntry Parse(string input)
        {
            int ptr = 0;
            return ParseListEntry(input, ref ptr);
        }

        private static ListEntry ParseListEntry(string input, ref int ptr)
        {
            if (input[ptr] != '[')
            {
                throw new Exception($"Expected [ at {ptr}, met {input[ptr]}");
            }
            ptr++;

            var entries = new List<IEntry>();

            while (input[ptr] != ']')
            {
                if (input[ptr] == '\'')
                {
                    ptr += 5;
                    entries.Add(new Null());
                }
                else if (input[ptr] == '[')
                {
                    entries.Add(ParseListEntry(input, ref ptr));
                }
                else
                {
                    var number = new Number(ParseNumber(input, ref ptr));
                    entries.Add(number);
                }

                if (input[ptr] == ',')
                    ptr++;
            }

            ptr++;
            return new ListEntry(entries.ToArray());
        }

        private static int ParseNumber(string input, ref int ptr)
        {
            var sb = new StringBuilder();
            while (char.IsDigit(input[ptr]) || input[ptr] == '-')
            {
                sb.Append(input[ptr++]);
            }

            return int.Parse(sb.ToString());
        }
    }
}