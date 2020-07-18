namespace Executor.Tree
{
    public class Variable : TreeNode
    {
        public Variable(string name)
        {
            Name = name;
        }
        
        public string Name { get; }
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Variable {Name}";
        public override string Print() => Name;
    }
}