using Executor.Tree;

namespace Executor.Reducers
{
    public abstract class UnaryOperatorReducer<T> : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (!(node is Application application))
                return node;
            if (!(application.Func is T))
                return node;
            return ReduceImpl(AstReducer.Reduce(application.Arg)) ?? node;
        }

        protected abstract TreeNode ReduceImpl(TreeNode arg);
    }
}