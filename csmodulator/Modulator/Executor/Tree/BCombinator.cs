namespace Executor.Tree
{
    public class BCombinator : TreeNode
    {
        private BCombinator()
        {
        }
        
        public static readonly BCombinator Instance = new BCombinator();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}B";
        public override string Print() => "b";
    }
}