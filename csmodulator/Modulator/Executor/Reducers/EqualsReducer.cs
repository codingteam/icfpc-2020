using Executor.Tree;

namespace Executor.Reducers
{
    public class EqualsReducer : BinaryOperatorReducer<Equal>
    {
        protected override TreeNode ReduceImpl(TreeNode x1, TreeNode x2)
        {
            if (x1 is Number n1 && x2 is Number n2)
                return n1.Value == n2.Value ? (TreeNode)  True.Instance :  False.Instance;
            if (x1 is Variable v1 && x2 is Variable v2)
                return v1 == v2 ? (TreeNode)  True.Instance :  False.Instance;

            return null;
        }
    }
}