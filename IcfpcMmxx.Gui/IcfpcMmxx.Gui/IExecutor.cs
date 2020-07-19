using System.Threading.Tasks;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public interface IExecutor
    {
        Task<ListCell> Interact(int dx, int dy);
    }
}
