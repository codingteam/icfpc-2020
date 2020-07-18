using Executor.Reducers;
using Executor.Tree;

namespace Executor
{
    public class AstReducer
    {
        public static TreeNode Reduce(TreeNode node)
        {
            var reducers = new IReducer[]
            {
                new AddReducer(),
                new MultReducer(),
                new DivReducer(),
                new IncReducer(),
                new DecReducer(),
                new EqualsReducer(),
                new LessThanReducer(),
                new NegationReducer(),
                new SCombinatorReducer(),
                new CCombinatorReducer(),
                new BCombinatorReducer(),
                new TrueReducer(),
                new FalseReducer(),
                new Power2Reducer(),
                new IdentityReducer(),
                new PairReducer(),
                new FirstReducer(),
                new TailReducer(),
                new NilReducer(),
                new IsNilReducer(),
                new IfZeroReducer(),
            };
            var changed = true;
            while (changed)
            {
                changed = false;
                foreach (var reducer in reducers)
                {
                    var result = reducer.Reduce(node);
                    if (result != node)
                    {
                        changed = true;
                        node = result;
                    }
                }
            }

            return node;
        }
    }
}