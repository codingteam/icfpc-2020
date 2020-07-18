using System;
using Modulator.Core;

namespace Modulator
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = "[0,-1,0]";
            var entry = Parser.Parse(input);
            var mo = entry.Modulate();
            Console.WriteLine(mo);
            var zzz = Demodulator.Demodulate(mo);
            zzz.Print(0);
        }
    }
}