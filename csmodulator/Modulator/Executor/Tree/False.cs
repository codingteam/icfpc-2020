namespace Executor.Tree
{
    public class False : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}False";
        public override string Print() => "f";
    }
}