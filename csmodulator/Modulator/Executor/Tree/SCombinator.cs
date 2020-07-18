namespace Executor.Tree
{
    public class SCombinator : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}S";
        public override string Print() => "s";
    }
}