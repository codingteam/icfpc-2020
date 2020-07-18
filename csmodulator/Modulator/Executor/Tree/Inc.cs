namespace Executor.Tree
{
    public class Inc : TreeNode
    {
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Inc";
        public override string Print() => "inc";
    }
}