namespace Executor.Tree
{
    public class True : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}True";
        public override string Print() => "t";
    }
}