using System;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Executor;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public struct InteractionResult
    {
        public NumberCell Flag { get; set; }
        public Cell State { get; set; }
        public ListCell Image { get; set; }
    }

    public class InteractorExecutor : IExecutor
    {
        public InteractorExecutor()
        {
            _interactorProcess = new Process
            {
                StartInfo =
                {
                    FileName = "stack",
                    ArgumentList =
                    {
                        "run", "interactor",
                    },
                    RedirectStandardError = true,
                    RedirectStandardOutput = true,
                    RedirectStandardInput = true,
                    WorkingDirectory = Program.MainDirectory
                }
            };
            _interactorProcess.ErrorDataReceived += (_, args) => Console.WriteLine($"STDERR: {args.Data}");

            _interactorProcess.Start();
            _interactorProcess.BeginErrorReadLine();
        }

        private Cell _state = null;
        private Process _interactorProcess;

        private static InteractionResult ParseInteractionResult(ListCell input)
        {
            var results = ListParser.EnumerateList(input).ToList();
            var flag = (NumberCell) results[0];
            var state = results[1];
            var image = (ListCell)results[2];
            if (results.Count > 3)
                Console.WriteLine("WARNING: more than 3 results");

            return new InteractionResult
            {
                Flag = flag,
                State = state,
                Image = image
            };
        }

        public async Task<(ListCell Images, string Raw)> Interact(int dx, int dy)
        {
            Console.WriteLine("sending");
            await _interactorProcess.StandardInput.WriteLineAsync($"{dx} {dy} {ListParser.Serialize(_state)}");
            Console.WriteLine("sent");

            var output = await _interactorProcess.StandardOutput.ReadLineAsync();
            var resultingData = output.Substring("+++".Length);
            Console.WriteLine(resultingData);
            var result = SetInteractionResult(resultingData);
            Console.WriteLine("FLAG: " + result.Flag.Value);
            return (result.Image, resultingData);
        }

        public InteractionResult SetInteractionResult(string raw)
        {
            var resultingAst = new AstParser(new FunctionDeclarationsFactory()).Parse(raw);
            var result = ParseInteractionResult((ListCell) ListParser.ParseAsList(resultingAst));
            _state = result.State;
            return result;
        }
    }
}
