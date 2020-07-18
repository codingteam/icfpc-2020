namespace Executor.Tree
{
    public class Add : TreeNode
    {
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Add";
        public override string Print() => "add";
    }
}