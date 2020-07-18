namespace Executor.Tree
{
    public class Inc : TreeNode
    {
        private Inc()
        {
        }
        
        public static readonly Inc Instance = new Inc();
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Inc";
        public override string Print() => "inc";
    }
}