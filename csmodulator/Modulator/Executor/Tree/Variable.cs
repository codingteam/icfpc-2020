using System.Collections.Generic;

namespace Executor.Tree
{
    public class Variable : TreeNode
    {
        private Variable(string name)
        {
            Name = name;
        }

        public static Variable GetOrCreate(string name)
        {
            if (myInstances.TryGetValue(name, out var result))
                return result;
            result = new Variable(name);
            myInstances.Add(name, result);
            return result;
        }
        
        public string Name { get; }
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Variable {Name}";
        public override string Print() => Name;

        private static Dictionary<string, Variable> myInstances = new Dictionary<string, Variable>();
    }
}