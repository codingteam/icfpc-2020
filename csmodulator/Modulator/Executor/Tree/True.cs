namespace Executor.Tree
{
    public class True : TreeNode
    {
        private True()
        {
        }
        
        public static readonly True Instance = new True();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}True";
        public override string Print() => "t";
    }
}