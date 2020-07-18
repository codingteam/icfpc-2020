using Executor.Tree;

namespace Executor.Reducers
{
    public class LessThanReducer : BinaryOperatorReducer<LessThan>
    {
        protected override TreeNode ReduceImpl(TreeNode x1, TreeNode x2)
        {
            if (x1 is Number n1 && x2 is Number n2)
                return n1.Value < n2.Value ? (TreeNode)  True.Instance :  False.Instance;

            return null;
        }
    }
}