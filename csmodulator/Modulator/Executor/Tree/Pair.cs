namespace Executor.Tree
{
    public class Pair : TreeNode
    {
        private Pair()
        {
        }
        
        public static readonly Pair Instance = new Pair();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Pair";
        public override string Print() => "cons";
    }
}