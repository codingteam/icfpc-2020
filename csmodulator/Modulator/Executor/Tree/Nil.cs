namespace Executor.Tree
{
    public class Nil : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Nil";
        public override string Print() => "nil";
    }
}