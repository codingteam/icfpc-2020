using System;
using System.Linq;

namespace Modulator.Core
{
    public class Pair : IEntry
    {
        public Pair(IEntry first, IEntry second)
        {
            First = first;
            Second = second;
        }

        public IEntry First { get; set; }
        public IEntry Second { get; set; }
        public string Modulate() => $"11{First.Modulate()}{Second.Modulate()}";

        public void Print(int level)
        {
            var tabs = string.Join("", Enumerable.Repeat("  ", level));
            Console.WriteLine($"{tabs}Pair");
            First.Print(level + 1);
            Second.Print(level + 1);
        }
    }
}