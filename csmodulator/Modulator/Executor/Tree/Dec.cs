namespace Executor.Tree
{
    public class Dec : TreeNode
    {
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Dec";
        public override string Print() => "dec";
    }
}