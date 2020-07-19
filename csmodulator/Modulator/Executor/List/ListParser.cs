using System;
using System.Collections.Generic;
using System.Globalization;
using Executor.Tree;

namespace Executor.List
{
    public static class ListParser
    {
        public static Cell ParseAsList(TreeNode node)
        {
            switch (node)
            {
                case Number n:
                    return new NumberCell(n.Value);
                case Application a:
                    var innerApp = (Application) a.Func;
                    var func = (Pair) innerApp.Func; // important to assert
                    var arg1 = ParseAsList(innerApp.Arg);
                    var arg2 = ParseAsList(a.Arg);
                    if (arg2 is null)
                        return new ListCell(arg1, null);
                    else if (arg2 is ListCell lc)
                        return new ListCell(arg1, lc);
                    else return new PairCell(arg1, arg2);
                case Nil _:
                    return null;
            }

            throw new Exception($"Unknown node type: {node}");
        }

        public static IEnumerable<Cell> EnumerateList(ListCell list)
        {
            var head = list;
            while (head != null)
            {
                yield return head.Datum;
                head = head.Tail;
            }
        }

        public static string Serialize(Cell cell)
        {
            return cell switch
            {
                null => "nil",
                NumberCell nc => nc.Value.ToString(CultureInfo.InvariantCulture),
                PairCell pc => "ap ap cons " + Serialize(pc.Item1) + " " + Serialize(pc.Item2),
                ListCell lc => "ap ap cons " + Serialize(lc.Datum) + " " + Serialize(lc.Tail)
            };
        }
    }
}
