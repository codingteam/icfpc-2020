namespace Executor.Tree
{
    public class Add : TreeNode
    {
        private Add()
        {
        }
        
        public static readonly Add Instance = new Add();
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Add";
        public override string Print() => "add";
    }
}