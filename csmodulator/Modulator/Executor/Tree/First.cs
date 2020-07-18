namespace Executor.Tree
{
    public class First : TreeNode
    {
        private First()
        {
        }
        
        public static readonly First Instance = new First();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}First";
        public override string Print() => "car";
    }
}