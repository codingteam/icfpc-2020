namespace Executor.Tree
{
    public class UserDefinedFunction : TreeNode
    {
        public UserDefinedFunction(string name)
        {
            Name = name;
        }

        public string Name { get; }
        public TreeNode Body { get; set; }

        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Invoke {Name}";

        public override string Print() => Name;
    }

    public class Add : TreeNode
    {
        private Add()
        {
        }

        public static readonly Add Instance = new Add();
        public override string PrettyPrint(int level) => $"{GenTabs(level)}Add";
        public override string Print() => "add";
    }
}
