namespace Executor.Tree
{
    public class Identity : TreeNode
    {
        private Identity()
        {
        }
        
        public static readonly Identity Instance = new Identity();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Identity";
        public override string Print() => "i";
    }
}