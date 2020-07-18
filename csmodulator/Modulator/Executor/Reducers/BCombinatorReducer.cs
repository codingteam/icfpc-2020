using Executor.Tree;

namespace Executor.Reducers
{
    public class BCombinatorReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            //ap (ap (ap b x0) x1) x2   =   ap x0 ap x1 x2
            if (node is Application a1
                && a1.Func is Application a2
                && a2.Func is Application a3
                && a3.Func is BCombinator)
                return new Application(
                    a3.Arg,
                    new Application(a2.Arg, a1.Arg)
                );
            return node;
        }
    }
}