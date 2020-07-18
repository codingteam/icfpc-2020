using System;
using Executor.Tree;

namespace Executor.Reducers
{
    public class IncReducer : UnaryOperatorReducer<Inc>
    {
        protected override TreeNode ReduceImpl(TreeNode arg)
        {
            if (!(arg is Number number))
                return null;
            return new Number(number.Value + 1);
        }
    }
}