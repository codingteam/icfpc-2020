namespace Executor.Tree
{
    public class Tail : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Tail";
        public override string Print() => "cdr";
    }
}