namespace Executor.Tree
{
    public class CCombinator : TreeNode
    {
        private CCombinator()
        {
        }
        
        public static readonly CCombinator Instance = new CCombinator();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}C";
        public override string Print() => "c";
    }
}