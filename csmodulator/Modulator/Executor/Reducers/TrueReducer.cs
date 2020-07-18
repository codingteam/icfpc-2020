using Executor.Tree;

namespace Executor.Reducers
{
    public class TrueReducer : BinaryOperatorReducer<True>
    {
        protected override TreeNode ReduceImpl(TreeNode x1, TreeNode x2)
        {
            return x1;
        }
    }
}