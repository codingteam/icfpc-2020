using System;
using Avalonia;
using Avalonia.Media;
using Avalonia.Media.Imaging;
using Avalonia.Platform;

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
            new PixelSize(640, 480),
            new Vector(96.0, 96.0),
            PixelFormat.Bgra8888);

        private void Draw(object image)
        {
            using var fb = Bitmap.Lock();

            // TODO: Draw the image

            _invalidate();
        }

        public unsafe void SetPixel(double dx, double dy, Color color)
        {
            var x = (int) dx;
            var y = (int) dy;

            var pixel = color.B + (color.G << 8) + (color.R << 16) +
                     (color.A << 24);

            using var fb = Bitmap.Lock();
            var ptr = (int*) fb.Address;
            ptr += Bitmap.PixelSize.Width * y + x;

            *ptr = pixel;

            _invalidate();
        }

        public void PixelClicked(double dx, double dy)
        {
            var image = _executor.Interact((int) dx, (int) dy);
            Draw(image);
        }
    }
}
