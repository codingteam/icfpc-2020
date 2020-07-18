using Executor.Tree;

namespace Executor.Reducers
{
    public class IsNilReducer : IReducer
    {
        public TreeNode Reduce(TreeNode node)
        {
            if (!(node is Application application))
                return node;
            if (!(application.Func is IsNil))
                return node;
            var arg = AstReducer.Reduce(application.Arg);
            if (arg is Nil)
                return True.Instance;
            return False.Instance;
        }
    }
}