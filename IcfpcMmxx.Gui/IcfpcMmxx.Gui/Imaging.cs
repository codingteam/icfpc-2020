using System.Collections.Generic;
using System.Linq;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public static class Imaging
    {
        public static IEnumerable<(int, int)> ToPixelList(ListCell image)
        {
            return ListParser.EnumerateList(image).Cast<PairCell>().Select(
                pair =>
                {
                    checked
                    {
                        var x = (int) ((NumberCell) pair.Item1).Value;
                        var y = (int) ((NumberCell) pair.Item2).Value;
                        return (x, y);
                    }
                });
        }
    }
}
