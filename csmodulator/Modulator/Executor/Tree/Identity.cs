namespace Executor.Tree
{
    public class Identity : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Identity";
        public override string Print() => "i";
    }
}