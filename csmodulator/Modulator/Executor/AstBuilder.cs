using System;
using System.Linq;
using Executor.Tree;

namespace Executor
{
    public class AstBuilder
    {
        public static TreeNode Build(string s)
        {
            var tokens = s.Split(' ').Where(x => !string.IsNullOrEmpty(x)).ToArray();
            var ptr = 0;
            return Build(tokens, ref ptr);
        }

        private static TreeNode Build(string[] tokens, ref int ptr)
        {
            var currentToken = tokens[ptr];
            ptr++;
            switch (currentToken)
            {
                case "ap":
                    return new Application(Build(tokens, ref ptr), Build(tokens, ref ptr));
                case "mul":
                    return new Mult();
                case "div":
                    return new Div();
                case "add":
                    return new Add();
                case "inc":
                    return new Inc();
                case "dec":
                    return new Dec();
                case "t":
                    return new True();
                case "f":
                    return new False();
                case "eq":
                    return new Equals();
                case "lt":
                    return new LessThan();
                case "neg":
                    return new Negate();
                case "s":
                    return new SCombinator();
                case "c":
                    return new CCombinator();
                case "b":
                    return new BCombinator();
                case "pwr2":
                    return new Power2();
                case "i":
                    return new Identity();
                case "cons":
                    return new Pair();
                case "car":
                    return new First();
                case "cdr":
                    return new Tail();
                case "nil":
                    return new Nil();
                case "isnil":
                    return new IsNil();
            }

            if (int.TryParse(currentToken, out var result))
                return new Number(result);

            if (currentToken.StartsWith("x"))
                return new Variable(currentToken);
            
            throw new Exception($"Unknown token {currentToken}");
        }
    }
}