namespace Executor.Tree
{
    public class Tail : TreeNode
    {
        private Tail()
        {
        }
        
        public static readonly Tail Instance = new Tail();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Tail";
        public override string Print() => "cdr";
    }
}