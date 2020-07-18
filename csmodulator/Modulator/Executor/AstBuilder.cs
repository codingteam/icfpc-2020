using System;
using System.Collections.Generic;
using System.Linq;
using Executor.Tree;

namespace Executor
{
    public class AstBuilder
    {
        public TreeNode Build(string s)
        {
            var tokens = s.Split(' ').Where(x => !string.IsNullOrEmpty(x)).ToArray();
            var ptr = 0;
            return Build(tokens, ref ptr);
        }

        private TreeNode Build(string[] tokens, ref int ptr)
        {
            var currentToken = tokens[ptr];
            ptr++;
            switch (currentToken)
            {
                case "ap":
                    return new Application(Build(tokens, ref ptr), Build(tokens, ref ptr));
                case "mul":
                    return Mult.Instance;
                case "div":
                    return Div.Instance;
                case "add":
                    return Add.Instance;
                case "inc":
                    return Inc.Instance;
                case "dec":
                    return Dec.Instance;
                case "t":
                    return True.Instance;
                case "f":
                    return False.Instance;
                case "eq":
                    return Equal.Instance;
                case "lt":
                    return LessThan.Instance;
                case "neg":
                    return Negate.Instance;
                case "s":
                    return SCombinator.Instance;
                case "c":
                    return CCombinator.Instance;
                case "b":
                    return BCombinator.Instance;
                case "pwr2":
                    return Power2.Instance;
                case "i":
                    return Identity.Instance;
                case "cons":
                case "vec":
                    return Pair.Instance;
                case "car":
                    return First.Instance;
                case "cdr":
                    return Tail.Instance;
                case "nil":
                    return Nil.Instance;
                case "isnil":
                    return IsNil.Instance;
                case "if0":
                    return IfZero.Instance;
            }

            if (int.TryParse(currentToken, out var result))
                return new Number(result);

            if (currentToken.StartsWith("x"))
                return Variable.GetOrCreate(currentToken);

            throw new Exception($"Unknown token {currentToken}");
        }
    }
}