using System.Linq;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Markup.Xaml;
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

        private IExecutor CreateExecutor() => new InteractorExecutor();

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            var grid = (Grid) Content;
            var image = grid.Children.First();

            _viewModel = new MainViewModel(
                () => Dispatcher.UIThread.InvokeAsync(() => image.InvalidateVisual()),
                CreateExecutor());

            image.PointerMoved += ImageOnPointerMoved;
            image.PointerPressed += ImageOnPointerPressed;

            _viewModel.PixelClicked(0, 0);
        }

        private (double, double) TranslatePosition(PointerEventArgs ea, Image image)
        {
            var pos = ea.GetPosition(image);
            var imageSize = (image.Bounds.Width, image.Bounds.Height);
            var sourceSize = _viewModel.Bitmap.PixelSize;
            var relativePos = (x: pos.X / imageSize.Width, y: pos.Y / imageSize.Height);
            var actualPos = (relativePos.x * sourceSize.Width, relativePos.y * sourceSize.Height);
            return actualPos;
        }

        private void ImageOnPointerMoved(object sender, PointerEventArgs e)
        {
            var (x, y) = TranslatePosition(e, (Image)sender);
            _viewModel.PixelHover(x, y);
        }

        private void ImageOnPointerPressed(object? sender, PointerPressedEventArgs e)
        {
            var (x, y) = TranslatePosition(e, (Image)sender);
            _viewModel.PixelClicked(x, y);
        }
    }
}
