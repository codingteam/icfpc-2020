namespace Executor.Tree
{
    public class Pair : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Pair";
        public override string Print() => "cons";
    }
}