using Executor.Tree;

namespace Executor.Reducers
{
    public class NegationReducer : UnaryOperatorReducer<Negate>
    {
        protected override TreeNode ReduceImpl(TreeNode arg)
        {
            if (!(arg is Number argument))
                return null;
            return new Number(-argument.Value);
        }
    }
}