namespace Executor.Tree
{
    public class BCombinator : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}B";
        public override string Print() => "b";
    }
}