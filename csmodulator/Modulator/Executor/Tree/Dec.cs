namespace Executor.Tree
{
    public class Dec : TreeNode
    {
        private Dec()
        {
        }
        
        public static readonly Dec Instance = new Dec();
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Dec";
        public override string Print() => "dec";
    }
}