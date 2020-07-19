using System.Threading.Tasks;

namespace IcfpcMmxx.Gui
{
    public interface IExecutor
    {
        Task<object> Interact(int dx, int dy);
    }
}
