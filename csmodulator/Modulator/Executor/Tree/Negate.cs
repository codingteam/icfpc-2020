namespace Executor.Tree
{
    public class Negate : TreeNode
    {
        private Negate()
        {
        }
        
        public static readonly Negate Instance = new Negate();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Negate";
        public override string Print() => "neg";
    }
}