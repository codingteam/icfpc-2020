using System;
using Modulator.Core;

namespace Modulator
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = "[[[-1,-3],[0,-3],[1,-3],[2,-2],[-2,-1],[-1,-1],[0,-1],[3,-1],[-3,0],[-1,0],[1,0],[3,0],[-3,1],[0,1],[1,1],[2,1],[-2,2],[-1,3],[0,3],[1,3],'nil'],[[-7,-2],[-7,-3],[-8,-2],[-8,-1],'nil'],'nil','nil']";
            var entry = Parser.Parse(input);
            Console.WriteLine(entry.Modulate());
        }
    }
}