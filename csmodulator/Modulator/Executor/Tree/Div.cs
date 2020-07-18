namespace Executor.Tree
{
    public class Div : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Div";
        public override string Print() => "div";
    }
}