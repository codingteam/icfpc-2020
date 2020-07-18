using System.Linq;

namespace Executor.Tree
{
    public abstract class TreeNode
    {
        protected string GenTabs(int level)
        {
            return string.Join("", Enumerable.Repeat("  ", level));
        }

        public abstract string PrettyPrint(int level = 0);
        public abstract string Print();

        public TreeNode Result { get; set; }
    }
}
