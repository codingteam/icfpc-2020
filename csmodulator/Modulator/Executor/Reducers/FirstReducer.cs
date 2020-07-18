using Executor.Tree;

namespace Executor.Reducers
{
    public class FirstReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (node is Application a1 && a1.Func is First)
            {
                // a1 car x2   =   ap x2 t
                if (!(a1.Arg is Application a2))
                    return new Application(a1.Arg, True.Instance);
                // a1 car (a2 (a3 cons x0) x1) = x0
                if (a2.Func is Application a3 && a3.Func is Pair)
                    return a3.Arg;
            }

            return node;
        }
    }
}