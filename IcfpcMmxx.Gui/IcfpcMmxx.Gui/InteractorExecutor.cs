using System;
using System.Diagnostics;
using System.Globalization;
using System.Threading.Tasks;
using Executor;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public class InteractorExecutor : IExecutor
    {
        private Cell _state = new ListCell();

        public async Task<ListCell> Interact(int dx, int dy)
        {
            var process = new Process
            {
                StartInfo =
                {
                    FileName = "stack",
                    ArgumentList =
                    {
                        "run", "interactor",
                        dx.ToString(CultureInfo.InvariantCulture),
                        dy.ToString(CultureInfo.InvariantCulture)
                    },
                    RedirectStandardError = true,
                    RedirectStandardOutput = true
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
            return (ListCell)ListParser.ParseAsList(resultingAst);
        }
    }
}
