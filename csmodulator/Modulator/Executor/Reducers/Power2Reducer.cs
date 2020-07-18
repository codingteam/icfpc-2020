using System;
using Executor.Tree;

namespace Executor.Reducers
{
    public class Power2Reducer : UnaryOperatorReducer<Power2>
    {
        protected override TreeNode ReduceImpl(TreeNode arg)
        {
            if (!(arg is Number number))
                return null;
            return new Number((long)Math.Pow(2, number.Value));
        }
    }
}