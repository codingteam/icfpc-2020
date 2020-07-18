namespace Executor.Tree
{
    public class CCombinator : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}C";
        public override string Print() => "c";
    }
}