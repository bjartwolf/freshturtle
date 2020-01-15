module LoadBitmap
open System.Drawing
open SharpDX
open SharpDX.Mathematics
open SharpDX.Direct2D1
open SharpDX.Direct3D10
open SharpDX.DXGI
open SharpDX.Windows

let Load(filename: string) (rndrTarget: RenderTarget)= 
    let bmp : System.Drawing.Bitmap = System.Drawing.Image.FromFile(filename) :?> System.Drawing.Bitmap 
    let bmpData =
        bmp.LockBits(
            System.Drawing.Rectangle(0, 0, bmp.Width, bmp.Height),
            System.Drawing.Imaging.ImageLockMode.ReadOnly,
            System.Drawing.Imaging.PixelFormat.Format32bppPArgb);

    let pointer = SharpDX.DataPointer(bmpData.Scan0, bmpData.Stride * bmpData.Height)
    let pFormat = SharpDX.Direct2D1.PixelFormat(SharpDX.DXGI.Format.B8G8R8A8_UNorm, Direct2D1.AlphaMode.Premultiplied);
    let bmpProps = SharpDX.Direct2D1.BitmapProperties(pFormat);

    let result = new SharpDX.Direct2D1.Bitmap(rndrTarget, SharpDX.Size2(bmp.Width, bmp.Height), pointer, bmpData.Stride, bmpProps); 

    bmp.UnlockBits(bmpData);
    bmp.Dispose();
    result
