namespace Executor.Tree
{
    public class IsNil : TreeNode
    {
        private IsNil()
        {
        }
        
        public static readonly IsNil Instance = new IsNil();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}IsNil";
        public override string Print() => "isnil";
    }
}