using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Modulator.Core
{
    public class Number : IEntry
    {
        public Number(int value)
        {
            Value = value;
        }

        public int Value { get; set; }

        public string Modulate()
        {
            return $"{(Value < 0 ? "10" : "01")}{Modulate(Math.Abs(Value))}";
        }

        public void Print(int level)
        {
            var tabs = string.Join("", Enumerable.Repeat("  ", level));
            Console.WriteLine($"{tabs}{Value}");
        }

        private string Modulate(int abs)
        {
            if (abs == 0)
                return "0";
            var size = 1;
            var bound = 16;
            while (bound <= abs)
            {
                bound *= 16;
                size++;
            }

            var sb = new StringBuilder();
            sb.Append(Enumerable.Repeat('1', size).ToArray());
            sb.Append('0');
            var list = new List<char>();
            while (bound > 1)
            {
                bound /= 2;
                list.Add(abs % 2 == 0 ? '0' : '1');
                abs /= 2;
            }

            list.Reverse();
            sb.Append(list.ToArray());
            return sb.ToString();
        }
    }
}