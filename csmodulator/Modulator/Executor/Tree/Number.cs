namespace Executor.Tree
{
    public class Number : TreeNode
    {
        public Number(long value)
        {
            Value = value;
        }

        public long Value { get; }
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Number {Value}";
        public override string Print() => $"{Value}";
    }
}