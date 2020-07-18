namespace Executor.Tree
{
    public class Negate : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Negate";
        public override string Print() => "neg";
    }
}