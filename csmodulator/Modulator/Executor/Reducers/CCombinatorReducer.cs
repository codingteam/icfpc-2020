using Executor.Tree;

namespace Executor.Reducers
{
    public class CCombinatorReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            //ap (ap (ap c x0) x1) x2   =   ap ap x0 x2 x1
            if (node is Application a1
                && a1.Func is Application a2
                && a2.Func is Application a3
                && a3.Func is CCombinator)
                return new Application(
                    new Application(a3.Arg, a1.Arg),
                    a2.Arg
                );
            return node;
        }
    }
}