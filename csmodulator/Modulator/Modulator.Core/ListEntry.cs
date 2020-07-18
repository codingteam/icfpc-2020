using System;

namespace Modulator.Core
{
    public class ListEntry : IEntry
    {
        public ListEntry(params IEntry[] entries)
        {
            Entries = entries;
        }

        public IEntry[] Entries { get; }

        public string Modulate()
        {
            return ModulateInternal(0);
        }

        private string ModulateInternal(int currentPosition)
        {
            if (Entries.Length == currentPosition)
                return "00";
            return $"11{Entries[currentPosition].Modulate()}{ModulateInternal(currentPosition + 1)}";
        }

        public void Print(int level)
        {
            throw new NotImplementedException();
        }
    }
}