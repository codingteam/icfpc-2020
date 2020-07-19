using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using Avalonia;
using Avalonia.Media;
using Avalonia.Media.Imaging;
using Avalonia.Platform;
using Executor;
using Executor.List;
using JetBrains.Annotations;

namespace IcfpcMmxx.Gui
{
    public class MainViewModel : INotifyPropertyChanged
    {
        private readonly Action _invalidate;
        private readonly IExecutor _executor;
        private (int, int) _minCoords = (0, 0);
        private (int, int) _lastClickCoords = (0, 0);
        private (int, int) _curCoords = (0, 0);
        private List<(int, int)> _clicks = new List<(int, int)>();


        public MainViewModel(Action invalidate, IExecutor executor)
        {
            _invalidate = invalidate;
            _executor = executor;
            _clicks = new List<(int, int)>();

            Bitmap = new WriteableBitmap(
                new PixelSize(320, 240),
                new Vector(96.0, 96.0),
                PixelFormat.Bgra8888);
        }

        private string _state;
        public string State
        {
            get => _state;
            set
            {
                _state = value;
                OnPropertyChanged(nameof(State));
            }
        }

        public async Task SetState()
        {
            var interactionResult = _executor.SetInteractionResult(State);
            await SetState(interactionResult.Image);
        }


        private WriteableBitmap _bitmap;
        public WriteableBitmap Bitmap
        {
            get => _bitmap;
            set
            {
                _bitmap = value;
                OnPropertyChanged();
            }
        }

        private string _info;
        private (int maxX, int maxY) _maxCoords;

        public string Info
        {
            get => _info;
            set
            {
                _info = value;
                OnPropertyChanged();
            }
        }

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

        private (int x, int y) ScreenToBitmap(int x, int y)
        {
            var (minX, minY) = _minCoords;
            return (-minX + x, -minY + y);
        }

        private (int x, int y) BitmapToScreen(int x, int y)
        {
            var (minX, minY) = _minCoords;
            return (x + minX, y + minY);
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
                    (x, y) = ScreenToBitmap(
                        (int) ((NumberCell) pixelData.Item1).Value,
                        (int) ((NumberCell) pixelData.Item2).Value);
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
            var ptr = (int*) fb.Address;
            ptr += Bitmap.PixelSize.Width * y + x;

            var oldPixel = *ptr;
            var pixel = color.B + (color.G << 8) + (color.R << 16) +
                     (color.A << 24);

            var resultPixel = oldPixel | pixel;
            *ptr = resultPixel;
        }

        private static (int, int, int, int) DetermineMinCoords(
            IEnumerable<ListCell> images)
        {
            var pixels = images.SelectMany(Imaging.ToPixelList);
            int? minX = null;
            int? minY = null;
            int? maxX = null;
            int? maxY = null;
            foreach (var p in pixels)
            {
                var (px, py) = p;
                if (minX == null || px < minX) minX = px;
                if (minY == null || py < minY) minY = py;
                if (maxX == null || px > maxX) maxX = px;
                if (maxY == null || py > maxY) maxY = py;
            }

            return (minX ?? 0, minY ?? 0, maxX ?? 0, maxY ?? 0);
        }

        private void RefreshInfo()
        {
            var (lastX, lastY) = _lastClickCoords;
            var (minX, minY) = _minCoords;
            var (maxX, maxY) = _maxCoords;
            var (x, y) = _curCoords;
            Info = $"Last click coords: {lastX}, {lastY}\n" +
                   $"Bitmap: ({minX}, {minY}) - ({maxX}, {maxY})\n" +
                   $"Current coords: {x}, {y}";
        }

        public async Task SetState(ListCell imageSet)
        {
            try
            {
                var images = ListParser.EnumerateList(imageSet).Cast<ListCell>().ToList();

                var (minX, minY, maxX, maxY) = DetermineMinCoords(images);
                _minCoords = (minX, minY);
                _maxCoords = (maxX, maxY);
                using (var oldBitmap = Bitmap)
                    Bitmap = new WriteableBitmap(
                        new PixelSize(maxX - minX + 1, maxY - minY + 1),
                        new Vector(96.0, 96.0), PixelFormat.Bgra8888);

                using var fb = Bitmap.Lock();
                ClearScreen(fb);
                Console.WriteLine($"{images.Count} images received");

                var colors = new[] {Colors.Red, Colors.Green, Colors.Blue};
                for (var i = 0; i < images.Count; i++)
                {
                    var image = images[i];
                    var color = colors[i % colors.Length];
                    Draw(fb, image, color);
                }

                RefreshInfo();
                _invalidate();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"ERROR: {ex}");
            }

        }

        public async Task ProcessClick(int x, int y)
        {
            try
            {
                _clicks.Add((x, y));
                _lastClickCoords = (x, y);
                var imageSet = await _executor.Interact(x, y);
                State = imageSet.Raw;
                await SetState(imageSet.Images);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"ERROR: {ex}");
            }
        }

        public async void PixelClicked(double dx, double dy)
        {
            try
            {
                var (x, y) = BitmapToScreen((int)dx, (int)dy);
                await ProcessClick(x, y);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"ERROR: {ex}");
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged(
            [CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this,
                new PropertyChangedEventArgs(propertyName));
        }

        public void PixelHover(double x, double y)
        {
            _curCoords = BitmapToScreen((int)x, (int)y);
            RefreshInfo();
        }
    }
}
