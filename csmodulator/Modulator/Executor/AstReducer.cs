using System;
using Executor.Reducers;
using Executor.Tree;

namespace Executor
{
    public class AstReducer
    {
        public static int ReducesCount = 0;

        public static TreeNode Reduce(TreeNode node)
        {
            if (node.Result != null)
                return node.Result;
            var init = node;
            while (true)
            {
                var result = TryReduce(node);
                if (result == node)
                {
                    init.Result = result;
                    return result;
                }
                node = result;
            }
        }

        private static TreeNode TryReduce(TreeNode node)
        {
            if (node.Result != null)
                return node.Result;
            if (node is UserDefinedFunction userDefinedFunction)
                return userDefinedFunction.Body;
            if (node is Application application1)
            {
                var fun = Reduce(application1.Func);
                var x = application1.Arg;
                if (fun is Negate)
                    return new Number(-Num(Reduce(x)).Value);
                if (fun is Identity)
                    return x;
                if (fun is Nil)
                    return True.Instance;
                if(fun is IsNil)
                    return new Application(x, new Application(True.Instance, new Application(True.Instance, False.Instance)));
                if(fun is First)
                    return new Application(x, True.Instance);
                if (fun is Tail)
                    return new Application(x, False.Instance);
                if (fun is Application application2)
                {
                    var fun2 = Reduce(application2.Func);
                    var y = application2.Arg;
                    if (fun2 is True)
                        return y;
                    if (fun2 is False)
                        return x;
                    if(fun2 is Add)
                        return new Number(Num(Reduce(x)).Value + Num(Reduce(y)).Value);
                    if(fun2 is Mult)
                        return new Number(Num(Reduce(x)).Value * Num(Reduce(y)).Value);
                    if(fun2 is Div)
                        return new Number(Num(Reduce(y)).Value / Num(Reduce(x)).Value);
                    if(fun2 is LessThan)
                        return Num(Reduce(y)).Value < Num(Reduce(x)).Value ? (TreeNode) True.Instance : False.Instance;
                    if(fun2 is Equal)
                        return Num(Reduce(x)).Value == Num(Reduce(y)).Value ? (TreeNode) True.Instance : False.Instance;
                    if (fun2 is Pair)
                        return ReducePair(y, x);
                    if (fun2 is Application application3)
                    {
                        var fun3 = Reduce(application3.Func);
                        var z = application3.Arg;
                        if(fun3 is SCombinator)
                            return new Application(new Application(z, x), new Application(y, x));
                        if(fun3 is CCombinator)
                            return new Application(new Application(z, x), y);
                        if(fun3 is BCombinator)
                            return new Application(z, new Application(y, x));
                        if(fun3 is Pair)
                            return new Application(new Application(x, z), y);
                    }
                }
            }

            return node;
        }

        private static TreeNode ReducePair(TreeNode a, TreeNode b)
        {
            var result = new Application(new Application(Pair.Instance, Reduce(a)), Reduce(b));
            result.Result = result;
            return result;
        }

        private static Number Num(TreeNode treeNode)
        {
            if (treeNode is Number number)
                return number;
            throw new Exception(
                $"Expected number, got {treeNode.PrettyPrint()}");
        }

        public static TreeNode Reduce2(TreeNode node)
        {
            var reducers = new IReducer[]
            {
                new AddReducer(),
                new MultReducer(),
                new DivReducer(),
                new IncReducer(),
                new DecReducer(),
                new EqualsReducer(),
                new LessThanReducer(),
                new NegationReducer(),
                new SCombinatorReducer(),
                new CCombinatorReducer(),
                new BCombinatorReducer(),
                new TrueReducer(),
                new FalseReducer(),
                new Power2Reducer(),
                new IdentityReducer(),
                new PairReducer(),
                new FirstReducer(),
                new TailReducer(),
                new NilReducer(),
                new IsNilReducer(),
                new IfZeroReducer(),
                new UserDefinedFunctionReducer(),
                new ApReducer(),
            };
            var changed = true;
            while (changed)
            {
                changed = false;
                foreach (var reducer in reducers)
                {
                    var result = reducer.Reduce(node);
                    if (result != node)
                    {
                        ReducesCount++;
                        changed = true;
                        node = result;
                    }
                }
            }

            return node;
        }
    }
}
