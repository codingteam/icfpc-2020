using System;
using System.Linq;

namespace Modulator.Core
{
    public class Null : IEntry
    {
        public string Modulate() => "00";

        public void Print(int level)
        {
            var tabs = string.Join("", Enumerable.Repeat("  ", level));
            Console.WriteLine($"{tabs}Null");
        }
    }
}