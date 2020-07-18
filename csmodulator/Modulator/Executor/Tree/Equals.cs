namespace Executor.Tree
{
    public class Equals : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Equals";
        public override string Print() => "eq";
    }
}