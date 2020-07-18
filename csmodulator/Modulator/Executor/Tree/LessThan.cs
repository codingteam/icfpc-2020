namespace Executor.Tree
{
    public class LessThan : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}LessThan";
        public override string Print() => "lt";
    }
}