namespace Executor.Tree
{
    public class First : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}First";
        public override string Print() => "car";
    }
}