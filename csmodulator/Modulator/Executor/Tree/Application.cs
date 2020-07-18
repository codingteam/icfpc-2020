namespace Executor.Tree
{
    public class Application : TreeNode
    {
        public Application(TreeNode func, TreeNode arg)
        {
            Func = func;
            Arg = arg;
        }

        public TreeNode Func { get; }
        public TreeNode Arg { get; }

        public override string PrettyPrint(int level) =>
            $"{GenTabs(level)}Ap\r\n" +
            $"{Func.PrettyPrint(level + 1)}\r\n" +
            $"{Arg.PrettyPrint(level + 1)}";

        public override string Print() => $"ap {Func.Print()} {Arg.Print()}";
    }
}