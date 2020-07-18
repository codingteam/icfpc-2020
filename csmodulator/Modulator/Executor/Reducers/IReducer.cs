using Executor.Tree;

namespace Executor.Reducers
{
    public interface IReducer
    {
        TreeNode Reduce(TreeNode node);
    }
}