namespace Demodulator
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = "1101100001110111110111011111100011100";
            var entry = Modulator.Core.Demodulator.Demodulate(input);
            entry.Print(0);
        }
    }
}