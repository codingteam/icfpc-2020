using System;
using NUnit.Framework;

namespace Executor.Tests
{
    public class SingleFuncTests
    {
        [SetUp]
        public void Setup()
        {
        }

        [TestCase("ap ap add 1 2", "3")]
        [TestCase("ap ap add 3 5", "8")]
        [TestCase("ap ap add 0 x0", "x0")]
        [TestCase("ap ap add x0 0", "x0")]
        [TestCase("ap ap mul 4 2", "8")]
        [TestCase("ap ap mul x0 0", "0")]
        [TestCase("ap ap mul x0 1", "x0")]
        [TestCase("ap inc 300", "301")]
        [TestCase("ap inc -6", "-5")]
        [TestCase("ap ap div 4 2", "2")]
        [TestCase("ap ap div 4 3", "1")]
        [TestCase("ap ap div 4 4", "1")]
        [TestCase("ap ap div 4 5", "0")]
        [TestCase("ap ap div 5 2", "2")]
        [TestCase("ap ap div 6 -2", "-3")]
        [TestCase("ap ap div 5 -3", "-1")]
        [TestCase("ap neg 0", "0")]
        [TestCase("ap neg 1", "-1")]
        [TestCase("ap neg -1", "1")]
        [TestCase("ap neg 2", "-2")]
        [TestCase("ap neg -2", "2")]
        [TestCase("ap pwr2 0", "1")]
        [TestCase("ap pwr2 1", "2")]
        [TestCase("ap pwr2 7", "128")]
        public void TestArithmetics(string input, string output) =>
            RunTest(input, output);

        [TestCase("ap ap eq x0 x0", "t")]
        [TestCase("ap ap eq 0 -2", "f")]
        [TestCase("ap ap eq 0 -1", "f")]
        [TestCase("ap ap eq 0 0", "t")]
        [TestCase("ap ap eq 0 1", "f")]
        [TestCase("ap ap lt 19 20", "t")]
        [TestCase("ap ap lt 20 20", "f")]
        [TestCase("ap ap lt 21 20", "f")]
        [TestCase("ap ap t x0 x1", "x0")]
        [TestCase("ap ap t 1 5", "1")]
        [TestCase("ap ap t t i", "t")]
        [TestCase("ap ap t t ap inc 5", "t")]
        [TestCase("ap ap t ap inc 5 t", "6")]
        [TestCase("ap ap f x0 x1", "x1")]
        public void TestBoolean(string input, string output) =>
            RunTest(input, output);

        [TestCase("ap ap ap s x0 x1 x2", "ap ap x0 x2 ap x1 x2")]
        [TestCase("ap ap ap s add inc 1", "3")]
        [TestCase("ap ap ap s mul ap add 1 6", "42")]
        [TestCase("ap ap ap c x0 x1 x2", "ap ap x0 x2 x1")]
        [TestCase("ap ap ap c add 1 2", "3")]
        [TestCase("ap ap ap b x0 x1 x2", "ap x0 ap x1 x2")]
        [TestCase("ap ap ap b inc dec 6", "6")]
        [TestCase("ap i x0", "x0")]
        [TestCase("ap i 1", "1")]
        [TestCase("ap i i", "i")]
        [TestCase("ap i add", "add")]
        [TestCase("ap i ap add 1", "ap add 1")]
        public void TestCombinators(string input, string output) =>
            RunTest(input, output);

        [TestCase("ap ap ap cons x0 x1 x2", "ap ap x2 x0 x1")]
        [TestCase("ap car ap ap cons x0 x1", "x0")]
        [TestCase("ap car x2", "ap x2 t")]
        [TestCase("ap car ap ap cons x0 ap ap cons x2 x1", "x0")]
        [TestCase("ap car ap ap cons ap ap cons x2 x1 nil", "ap ap cons x2 x1")]
        [TestCase("ap cdr ap ap cons x0 x1", "x1")]
        [TestCase("ap cdr x2", "ap x2 f")]
        [TestCase("ap cdr ap ap cons x0 ap ap cons x2 x1", "ap ap cons x2 x1")]
        [TestCase("ap cdr ap ap cons ap ap cons x2 x1 nil", "nil")]
        [TestCase("ap nil x0", "t")]
        [TestCase("ap isnil nil", "t")]
        [TestCase("ap isnil ap ap cons x0 x1", "f")]
        [TestCase("ap isnil ap ap t nil ap ap cons x0 x1", "t")]
        [TestCase("ap isnil ap ap f nil ap ap cons x0 x1", "f")]
        public void TestLists(string input, string output) =>
            RunTest(input, output);

        [TestCase("ap ap ap if0 0 x0 x1", "x0")]
        [TestCase("ap ap ap if0 1 x0 x1", "x1")]
        public void TestIfZero(string input, string output) =>
            RunTest(input, output);

        [TestCase("ap inc ap inc 0", "2")]
        [TestCase("ap inc ap inc ap inc 0", "3")]
        [TestCase("ap inc ap dec x0", "x0")]
        [TestCase("ap dec ap inc x0", "x0")]
        [TestCase("ap dec ap ap add x0 1", "x0")]
        [TestCase("ap ap add ap ap add 2 3 4", "9")]
        [TestCase("ap ap add 2 ap ap add 3 4", "9")]
        [TestCase("ap ap add ap ap mul 2 3 4", "10")]
        [TestCase("ap ap mul 2 ap ap add 3 4", "14")]
        public void TestApplication(string input, string output) =>
            RunTest(input, output);

        private void RunTest(string input, string output)
        {
            var ast =
                new AstParser(new FunctionDeclarationsFactory()).Parse(input);
            var reduced = AstReducer.Reduce(ast);
            var expected =
                new AstParser(new FunctionDeclarationsFactory()).Parse(output);
            Console.WriteLine("<<<<<<<<Input ast>>>>>>>>");
            Console.WriteLine(ast.PrettyPrint());
            Console.WriteLine("<<<<<<<<Reduced ast>>>>>>>>");
            Console.WriteLine(reduced.PrettyPrint());
            Console.WriteLine("<<<<<<<<Expected ast>>>>>>>>");
            Console.WriteLine(expected.PrettyPrint());
            Assert.AreEqual(output, reduced.Print());
        }
    }
}