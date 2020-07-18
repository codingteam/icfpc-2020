using Executor.Tree;

namespace Executor.Reducers
{
    public class TailReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (node is Application a1 && a1.Func is Tail)
            {            
                // a1 cdr x2   =   ap x2 f
                if (!(a1.Arg is Application a2))
                    return new Application(a1.Arg, False.Instance);
                // a1 cdr (a2 (a3 cons x0) x1)   =   x1
                if (a2.Func is Application a3 && a3.Func is Pair)
                    return a2.Arg;
            }

            return node;
        }
    }
}