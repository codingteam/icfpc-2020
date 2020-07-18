using Executor.Tree;

namespace Executor.Reducers
{
    public class PairReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            //ap (ap (ap cons x0) x1) x2)   =   ap ap x2 x0 x1
            if (node is Application a1 && a1.Func is Application a2 && a2.Func is Application a3 && a3.Func is Pair)
                return new Application(new Application(a1.Arg, a3.Arg), a2.Arg);
            return node;
        }
    }
}