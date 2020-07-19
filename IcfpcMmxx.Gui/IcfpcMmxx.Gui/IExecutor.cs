using System.Threading.Tasks;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public interface IExecutor
    {
        Task<(ListCell Images, string Raw)> Interact(int dx, int dy);
        InteractionResult SetInteractionResult(string raw);
    }
}
