using System.Numerics;

namespace Executor.Tree
{
    public class Number : TreeNode
    {
        public Number(BigInteger value)
        {
            Value = value;
        }

        public BigInteger Value { get; }
        public override string PrettyPrint(int level = 0) => $"{GenTabs(level)}Number {Value}";
        public override string Print() => $"{Value}";
    }
}
