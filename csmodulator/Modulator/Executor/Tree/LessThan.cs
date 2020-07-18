namespace Executor.Tree
{
    public class LessThan : TreeNode
    {
        private LessThan()
        {
        }
        
        public static readonly LessThan Instance = new LessThan();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}LessThan";
        public override string Print() => "lt";
    }
}