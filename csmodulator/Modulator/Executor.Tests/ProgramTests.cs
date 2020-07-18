using System;
using System.IO;
using NUnit.Framework;

namespace Executor.Tests
{
    public class ProgramTests
    {
        [Test, Explicit]
        public void Test()
        {
            var add4 = "add4 = ap add ap add ap add ap add 4";
            var main = "ap add4 8 5 5 5 5 5 3 3 3";
            var fff = "fff = ap t fff";
            var main1 = "ap fff 42";
            var executor = new ProgramExecutor();
            var result = executor.Execute(main, add4);
            //var result = executor.Execute(main1, fff);N
            Console.WriteLine(result.PrettyPrint());
            Console.WriteLine(result.Print());
            Console.WriteLine(AstReducer.ReducesCount);
        }

        [Test]
        public void RunGalaxy()
        {
            var declarations = File.ReadAllLines("galaxy.txt");
            var main = "ap galaxy ap ap cons 0 146";
            var executor = new ProgramExecutor();
            var result = executor.Execute(main, declarations);
            TestContext.Progress.WriteLine(result.PrettyPrint());
        }

        [Test, Explicit]
        public void Test1()
        {
            var x = "ap ap s s ap ap ap s t x0 x1";
            Console.WriteLine(AstReducer.Reduce(new AstParser(new FunctionDeclarationsFactory()).Parse(x)).PrettyPrint());
        }
    }
}
