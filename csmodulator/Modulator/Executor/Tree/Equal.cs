namespace Executor.Tree
{
    public class Equal : TreeNode
    {
        private Equal()
        {
        }
        
        public static readonly Equal Instance = new Equal();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Equal";
        public override string Print() => "eq";
    }
}