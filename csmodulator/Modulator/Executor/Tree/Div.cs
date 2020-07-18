namespace Executor.Tree
{
    public class Div : TreeNode
    {
        private Div()
        {
        }
        
        public static readonly Div Instance = new Div();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Div";
        public override string Print() => "div";
    }
}