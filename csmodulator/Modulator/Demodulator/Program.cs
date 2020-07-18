namespace Demodulator
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = "1101100001110111110110111001010100000";
            var entry = Modulator.Core.Demodulator.Demodulate(input);
            entry.Print(0);
        }
    }
}