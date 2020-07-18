using Executor.Tree;

namespace Executor.Reducers
{
    public abstract class BinaryOperatorReducer<T> : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (!(node is Application application))
                return node;
            if (!(application.Func is Application nested))
                return node;
            if (!(nested.Func is T))
                return node;
            return ReduceImpl(AstReducer.Reduce(nested.Arg), AstReducer.Reduce(application.Arg)) ?? node;
        }

        protected abstract TreeNode ReduceImpl(TreeNode x1, TreeNode x2);
    }
}