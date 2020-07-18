using Executor.Tree;

namespace Executor.Reducers
{
    public class FalseReducer : BinaryOperatorReducer<False>
    {
        protected override TreeNode ReduceImpl(TreeNode x1, TreeNode x2)
        {
            return x2;
        }
    }
}