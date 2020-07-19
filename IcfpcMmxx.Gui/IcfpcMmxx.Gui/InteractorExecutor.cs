using System;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading.Tasks;
using Executor;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public class InteractorExecutor : IExecutor
    {
        private Cell _state = null;

        private struct InteractionResult
        {
            public NumberCell Flag { get; set; }
            public Cell State { get; set; }
            public ListCell Image { get; set; }
        }

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

        public async Task<ListCell> Interact(int dx, int dy)
        {
            Console.WriteLine("Starting app stack in directory: " + Program.MainDirectory);
            var process = new Process
            {
                StartInfo =
                {
                    FileName = "stack",
                    ArgumentList =
                    {
                        "run", "--", "interactor",
                        ListParser.Serialize(_state),
                        dx.ToString(CultureInfo.InvariantCulture),
                        dy.ToString(CultureInfo.InvariantCulture)
                    },
                    RedirectStandardError = true,
                    RedirectStandardOutput = true,
                    WorkingDirectory = Program.MainDirectory
                }
            };

            string resultingData = null;
            process.OutputDataReceived += (_, args) =>
            {
                Console.WriteLine($"STDOUT: {args.Data}");
                if (args.Data?.StartsWith("+++") == true)
                {
                    resultingData = args.Data.Substring("+++".Length);
                }
            };
            process.ErrorDataReceived += (_, args) => Console.WriteLine($"STDERR: {args.Data}");

            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();
            var exitCode = await Task.Run(() =>
            {
                process.WaitForExit();
                return process.ExitCode;
            });

            Console.WriteLine($"EXIT CODE: {exitCode}");
            Console.WriteLine($"Resulting data: {resultingData}");

            var resultingAst = new AstParser(new FunctionDeclarationsFactory()).Parse(resultingData);
            var result = ParseInteractionResult((ListCell) ListParser.ParseAsList(resultingAst));
            Console.WriteLine("FLAG: " + result.Flag.Value);
            _state = result.State;
            return result.Image;
        }
    }
}
