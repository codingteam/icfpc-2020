namespace Executor.Tree
{
    public class Mult : TreeNode
    {
        private Mult()
        {
        }
        
        public static readonly Mult Instance = new Mult();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Mult";
        public override string Print() => "mul";
    }
}