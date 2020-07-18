using System.Linq;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Markup.Xaml;
using Avalonia.Media;
using Avalonia.Threading;

namespace IcfpcMmxx.Gui
{
    public class MainWindow : Window
    {
        private MainViewModel _viewModel;

        public MainWindow()
        {
            InitializeComponent();
            this.AttachDevTools();
            DataContext = _viewModel;
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            var grid = (Grid) Content;
            var image = grid.Children.First();

            _viewModel = new MainViewModel(
                () => Dispatcher.UIThread.InvokeAsync(() => image.InvalidateVisual()));

            image.PointerMoved += ImageOnPointerMoved;
            image.PointerPressed += ImageOnPointerPressed;
        }

        private (double, double) TranslatePosition(PointerEventArgs ea, Image image)
        {
            var pos = ea.GetPosition(image);
            return (pos.X, pos.Y);
        }

        private void ImageOnPointerMoved(object sender, PointerEventArgs e)
        {
            var (x, y) = TranslatePosition(e, (Image)sender);
            _viewModel.SetPixel(x, y, Colors.White);
        }

        private void ImageOnPointerPressed(object? sender, PointerPressedEventArgs e)
        {
            var (x, y) = TranslatePosition(e, (Image)sender);
            _viewModel.PixelClicked(x, y);
        }
    }
}
