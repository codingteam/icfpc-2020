namespace Executor.Tree
{
    public class Mult : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Mult";
        public override string Print() => "mul";
    }
}