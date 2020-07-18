namespace Executor.Tree
{
    public class IsNil : TreeNode
    {
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}IsNil";
        public override string Print() => "isnil";
    }
}