using System.IO;
using System.Reflection;
using Avalonia;
using Avalonia.Logging.Serilog;

namespace IcfpcMmxx.Gui
{
    class Program
    {
        public static string MainDirectory;

        // Initialization code. Don't use any Avalonia, third-party APIs or any
        // SynchronizationContext-reliant code before AppMain is called: things aren't initialized
        // yet and stuff might break.
        public static void Main(string[] args)
        {
            MainDirectory = args.Length > 0
                ? args[0]
                : Path.GetFullPath(Path.Combine(
                    Assembly.GetExecutingAssembly().Location, "..", "..", "..", "..", "..", ".."));
            BuildAvaloniaApp()
                .StartWithClassicDesktopLifetime(args);
        }

        // Avalonia configuration, don't remove; also used by visual designer.
        public static AppBuilder BuildAvaloniaApp()
            => AppBuilder.Configure<App>()
                .UsePlatformDetect()
                .LogToDebug();
    }
}
