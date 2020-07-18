namespace Modulator.Core
{
    public interface IEntry
    {
        string Modulate();
        void Print(int level);
    }
}