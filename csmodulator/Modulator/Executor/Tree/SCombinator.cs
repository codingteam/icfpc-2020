namespace Executor.Tree
{
    public class SCombinator : TreeNode
    {
        private SCombinator()
        {
        }
        
        public static readonly SCombinator Instance = new SCombinator();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}S";
        public override string Print() => "s";
    }
}