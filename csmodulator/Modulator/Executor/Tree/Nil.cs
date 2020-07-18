namespace Executor.Tree
{
    public class Nil : TreeNode
    {
        private Nil()
        {
        }
        
        public static readonly Nil Instance = new Nil();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Nil";
        public override string Print() => "nil";
    }
}