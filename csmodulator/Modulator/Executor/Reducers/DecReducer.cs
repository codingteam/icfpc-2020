using Executor.Tree;

namespace Executor.Reducers
{
    public class DecReducer : UnaryOperatorReducer<Dec>
    {
        protected override TreeNode ReduceImpl(TreeNode arg)
        {
            if (!(arg is Number number))
                return null;
            return new Number(number.Value - 1);
        }
    }
}