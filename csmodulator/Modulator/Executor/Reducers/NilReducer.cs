using Executor.Tree;

namespace Executor.Reducers
{
    public class NilReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (!(node is Application application))
                return node;
            if (!(application.Func is Nil))
                return node;
            return new True();
        }
    }
}