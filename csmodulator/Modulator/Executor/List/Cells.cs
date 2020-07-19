using System.Numerics;

namespace Executor.List
{
    public abstract class Cell {}

    public class ListCell : Cell
    {
        public Cell Datum { get; }
        public ListCell? Tail { get; }
        public ListCell(Cell datum, ListCell? tail)
        {
            Datum = datum;
            Tail = tail;
        }
    }

    public class NumberCell : Cell
    {
        public BigInteger Value { get; }
        public NumberCell(BigInteger value)
        {
            Value = value;
        }
    }

    public class PairCell : Cell
    {
        public Cell Item1 { get; }
        public Cell Item2 { get; }

        public PairCell(Cell item1, Cell item2)
        {
            Item1 = item1;
            Item2 = item2;
        }
    }
}
