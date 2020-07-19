using System;
using System.Linq;
using Avalonia;
using Avalonia.Media;
using Avalonia.Media.Imaging;
using Avalonia.Platform;
using Executor.List;

namespace IcfpcMmxx.Gui
{
    public class MainViewModel
    {
        private readonly Action _invalidate;
        private readonly IExecutor _executor;

        public MainViewModel(Action invalidate, IExecutor executor)
        {
            _invalidate = invalidate;
            _executor = executor;
        }

        public WriteableBitmap Bitmap { get; } = new WriteableBitmap(
            new PixelSize(320, 240),
            new Vector(96.0, 96.0),
            PixelFormat.Bgra8888);

        private void ClearScreen(ILockedFramebuffer fb)
        {
            var size = Bitmap.PixelSize;
            for (int y = 0; y < size.Height; ++y)
            {
                for (int x = 0; x < size.Width; ++x)
                {
                    var color = Colors.Black;
                    SetPixel(fb, x, y, color);
                }
            }
        }

        private void Draw(ILockedFramebuffer fb, ListCell image, Color color)
        {
            var size = Bitmap.PixelSize;
            foreach (var pixelCell in ListParser.EnumerateList(image))
            {
                var pixelData = (PairCell) pixelCell;
                int x, y;
                checked
                {
                    x = 100 + (int)((NumberCell) pixelData.Item1).Value;
                    y = 100 + (int)((NumberCell) pixelData.Item2).Value;
                }

                if (x < 0 || y < 0 || x >= size.Width || y >= size.Height)
                {
                    Console.WriteLine($"WARN: x = {x}, y = {y}; NOT drawing");
                    continue;
                }

                SetPixel(fb, x, y, color);
            }
        }

        private unsafe void SetPixel(ILockedFramebuffer fb, int x, int y, Color color)
        {
            var pixel = color.B + (color.G << 8) + (color.R << 16) +
                     (color.A << 24);

            var ptr = (int*) fb.Address;
            ptr += Bitmap.PixelSize.Width * y + x;

            *ptr = pixel;
        }

        public async void PixelClicked(double dx, double dy)
        {
            try
            {
                var imageSet = await _executor.Interact((int) dx, (int) dy);
                using var fb = Bitmap.Lock();
                ClearScreen(fb);
                var images = ListParser.EnumerateList(imageSet).ToList();
                Console.WriteLine($"{images.Count} images received");
                var colors = new[] {Colors.Red, Colors.Green, Colors.Blue};
                for (var i = 0; i < images.Count; i++)
                {
                    var image = (ListCell)images[i];
                    var color = colors[i];
                    Draw(fb, image, color);
                }

                _invalidate();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"ERROR: {ex}");
            }
        }
    }
}
