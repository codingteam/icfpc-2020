using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Executor.Tree;

namespace Executor
{
    public class FunctionDeclarationsFactory
    {
        public UserDefinedFunction GetOrCreate(string name)
        {
            if (myDeclarations.TryGetValue(name, out var result))
                return result;
            result = new UserDefinedFunction(name);
            myDeclarations.Add(name, result);
            return result;
        }

        public void ValidateFullness()
        {
            foreach(var (name, body) in myDeclarations)
                if (body == null)
                    throw new Exception($"Function {name} is used but not declared");
        }

        private Dictionary<string, UserDefinedFunction> myDeclarations = new Dictionary<string, UserDefinedFunction>();
    }

    public class ProgramExecutor
    {
        public TreeNode Execute(string func, params string[] declarations)
        {
            var functionDeclarationsFactory = new FunctionDeclarationsFactory();
            var parser = new AstParser(functionDeclarationsFactory);
            foreach (var declaration in declarations)
            {
                var spl = declaration.Split(" = ");
                var funcName = spl[0];
                var funcBody = spl[1];
                var functionDeclaration =
                    functionDeclarationsFactory.GetOrCreate(funcName);
                var body = parser.Parse(funcBody);
                functionDeclaration.Body = body;
            }

            var parsedFunc = parser.Parse(func);
            functionDeclarationsFactory.ValidateFullness();

            return AstReducer.Reduce(parsedFunc);
        }
    }

    public class AstParser
    {
        private readonly FunctionDeclarationsFactory myFunctionDeclarationsFactory;

        public AstParser(FunctionDeclarationsFactory functionDeclarationsFactory)
        {
            myFunctionDeclarationsFactory = functionDeclarationsFactory;
        }

        public TreeNode Parse(string s)
        {
            var tokens = s.Split(' ').Where(x => !string.IsNullOrEmpty(x)).ToArray();
            var ptr = 0;
            return Parse(tokens, ref ptr);
        }

        private TreeNode Parse(string[] tokens, ref int ptr)
        {
            if (ptr >= tokens.Length)
            {
                Console.WriteLine(string.Join(" ", tokens));
            }
            var currentToken = tokens[ptr];
            ptr++;
            switch (currentToken)
            {
                case "ap":
                    return new Application(Parse(tokens, ref ptr), Parse(tokens, ref ptr));
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

            if (BigInteger.TryParse(currentToken, out var result))
                return new Number(result);

            if (currentToken.StartsWith("x"))
                return Variable.GetOrCreate(currentToken);

            return myFunctionDeclarationsFactory.GetOrCreate(currentToken);
        }
    }
}
