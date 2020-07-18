namespace Executor.Tree
{
    public class Power2 : TreeNode
    {
        private Power2()
        {
        }
        
        public static readonly Power2 Instance = new Power2();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Power2";
        public override string Print() => "pwr2";
    }
}