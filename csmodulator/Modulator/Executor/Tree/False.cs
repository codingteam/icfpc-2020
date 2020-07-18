namespace Executor.Tree
{
    public class False : TreeNode
    {
        private False()
        {
        }

        public static readonly False Instance = new False();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}False";
        public override string Print() => "f";
    }

    public class IfZero : TreeNode
    {
        private IfZero()
        {
        }

        public static readonly IfZero Instance = new IfZero();
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}IfZero";
        public override string Print() => "if0";
    }
}