using Executor.Tree;

namespace Executor.Reducers
{
    public class DivReducer : BinaryOperatorReducer<Div>
    {
        protected override TreeNode ReduceImpl(TreeNode x1, TreeNode x2)
        {
            var n1 = x1 as Number;
            var n2 = x2 as Number;

            if (n1 == null && n2 != null && n2.Value == 1)
                return x1;
            if (n2 == null && n1 != null && n1.Value == 1)
                return x2;

            if (n1 == null || n2 == null)
                return null;

            return new Number(n1.Value / n2.Value);
        }
    }
}