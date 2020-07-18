using Executor.Tree;

namespace Executor.Reducers
{
    public class IdentityReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (!(node is Application application))
                return node;
            if (!(application.Func is Identity))
                return node;
            return application.Arg;
        }
    }
}