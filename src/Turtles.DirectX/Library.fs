open System
open System.Drawing
open System.Windows
open SharpDX
open SharpDX.Mathematics
open SharpDX.Direct2D1
open SharpDX.Direct3D10
open SharpDX.DXGI
open SharpDX.Windows
open Fish
open Boxes 
open NAudio.Midi 
open Limited 
open ScreenSettings

open My.Turtles
open SharpDX.Direct2D1.Effects

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let form = new RenderForm("Fish", Size = Size(ScreenRes.x_max, ScreenRes.y_max))
    let mutable x = 0
    let mutable midi_16 = 0
    let mutable midi_17 = 50
    let mutable midi_18 = 2
    let mutable midi_19 = 20
    let mutable midi_20 = 0
    let mutable midi_21 = 0
    let mutable midi_22 = 0
    let mutable midi_23 = 0
    let midiIn = new MidiIn(0);
    midiIn.MessageReceived.Add(fun e -> 
            let evt = MidiEvent.FromRawMessage(e.RawMessage);
            if (evt :? ControlChangeEvent) then 
                let foo = evt :?> ControlChangeEvent
                sprintf "Kanal %A  - Value: %A" foo.Controller  foo.ControllerValue |> Console.WriteLine
                if (int foo.Controller = 19) then
                    midi_19 <- foo.ControllerValue 
                else if (int foo.Controller = 16) then
                    midi_16 <- foo.ControllerValue 
                else if (int foo.Controller = 17) then
                    midi_17 <- foo.ControllerValue 
                else if (int foo.Controller = 18) then
                    midi_18 <- foo.ControllerValue 
                else if (int foo.Controller = 20) then
                    midi_20 <- foo.ControllerValue 
                else if (int foo.Controller = 21) then
                    midi_21 <- foo.ControllerValue 
                else if (int foo.Controller = 22) then
                    midi_22 <- foo.ControllerValue 
                else if (int foo.Controller = 23) then
                    midi_23 <- foo.ControllerValue 
    ) 

    midiIn.Start();


    let desc = SwapChainDescription (
                BufferCount = 1,
                ModeDescription =
                    ModeDescription(form.ClientSize.Width, form.ClientSize.Height,
                        Rational(60, 1), Format.R8G8B8A8_UNorm),
                OutputHandle = form.Handle,
                SampleDescription = SampleDescription(1, 0),
                SwapEffect = SwapEffect.Discard,
                IsWindowed = Interop.RawBool(true),
                Usage = Usage.RenderTargetOutput)
    let mutable device: SharpDX.Direct3D10.Device1 = null
    let swapChain = ref null 
    SharpDX.Direct3D10.Device1.CreateWithSwapChain(DriverType.Hardware, DeviceCreationFlags.BgraSupport, desc,
                SharpDX.Direct3D10.FeatureLevel.Level_10_1, &device, swapChain) 

    use d2DFactory = new Direct2D1.Factory()
    use factory = (!swapChain).GetParent<Factory>()
    factory.MakeWindowAssociation(form.Handle, WindowAssociationFlags.IgnoreAll)
    use backBuffer = Resource.FromSwapChain<Texture2D>((!swapChain), 0)
    use renderTrgt = new RenderTargetView(device, backBuffer)
    let surface = backBuffer.QueryInterface<Surface>()
    let d2DRenderTarget = new RenderTarget(d2DFactory, surface, 
                                  RenderTargetProperties(
                                    PixelFormat(Format.Unknown, Direct2D1.AlphaMode.Premultiplied)))

    let hotpink = Color.HotPink.ToVector3()
    
    let pink = Interop.RawColor4(hotpink.X, hotpink.Y, hotpink.Z, 50.0f)
    
    let pinkBrush = new SolidColorBrush(d2DRenderTarget, pink, BrushProperties(Opacity = 0.30f) |> Nullable<BrushProperties>)

    let matrixToRaw (mtrx: Matrix3x2) =
        Interop.RawMatrix3x2(mtrx.M11, mtrx.M12, mtrx.M21, mtrx.M22, mtrx.M31, mtrx.M32)

    let translate (translationx: float32) (translationy: float32)= 
        let center = Vector2(translationx, translationy)
        Matrix3x2.Translation(center)

    let scaleOrigo (scalex:float32) (scaley: float32) =
        Matrix3x2.Scaling(scalex, scaley)

    let scale (scalex:float32) (scaley: float32) (center: Vector2)= 
        Matrix3x2.Scaling(scalex, scaley, center)

    ///<summary>Missing rotation point?</summary>
    let skew(angleX:float32) (angleY: float32) = 
        Matrix3x2.Skew(angleX, angleY)

    let rotate angle (point : Vector2) = Matrix3x2.Rotation(angle, point)
     
    let emptyGeo = new PathGeometry(d2DFactory)
    let emptySink = emptyGeo.Open()
    emptySink.Close()

    let fishGeo = new PathGeometry(d2DFactory) 
    let sink = fishGeo.Open()
    sink.SetFillMode(Direct2D1.FillMode.Alternate)
    for (start, bezierCurve) in hendersonFishCurves do
        let start = Interop.RawVector2(start.X, start.Y)
        sink.BeginFigure(start, FigureBegin.Hollow  )
        sink.AddBezier(bezierCurve)
        sink.EndFigure(FigureEnd.Open)
    let foo = sink.Close()
    let boxToMtrx (box: Box) : Matrix3x2 = 
        let bLength = box.b.Length()
        let cLength = box.c.Length()
        let dotProdBC = Vector2.Dot(box.b,box.c)
        let angleBC = acos (dotProdBC / (bLength * cLength )) 
        let delta = box.c.X * box.b.Y - box.c.Y*box.b.X 
        let cScale = if delta < 0.0f then
                        cLength
                     else 
                        -cLength

//        Console.WriteLine(sprintf "Vector a: %A Vector b: %A Vector c: %A " box.a box.b box.c)
//        Console.WriteLine(sprintf "delta: %f --- angleBC: %f ----- cScale:  %f" delta angleBC cScale)
        let rotAngleB = float32 (Math.Atan2(float box.b.Y, float box.b.X))
//        transform (rotate rotAngleB box.a) geo
        //transform ((rotate rotAngleB box.a) * (scale bLength cLength box.a) * (translate box.a.X box.a.Y)) geo
        let mtrx = (scaleOrigo bLength cScale) * (translate box.a.X box.a.Y) * (rotate rotAngleB box.a)
        mtrx

    let grouper (factory: Direct2D1.Factory) (geos: Geometry []) = 
        new GeometryGroup(factory, FillMode.Alternate, geos)

    let bitmapBrush = new BitmapBrush(d2DRenderTarget, LoadBitmap.Load "image.jpg" d2DRenderTarget)
    let draw (geo: Geometry) (brush: Brush) = d2DRenderTarget.DrawGeometry(geo, brush, 0.9f)
    let drawWithBrush (geo: Geometry) = draw geo
    let draw (geo: Geometry) = d2DRenderTarget.DrawGeometry(geo, bitmapBrush)

    let transform : Box -> Geometry -> Geometry = 
      let transformer (factory: Direct2D1.Factory) (box: Box) (geo : Geometry) : Geometry =
        let mtrx = boxToMtrx box
        new TransformedGeometry(factory, geo, mtrx |> matrixToRaw) :> Geometry
      transformer d2DFactory 

    let rotateStep : float32 -> Geometry -> Geometry = 
      let transformer (factory: Direct2D1.Factory) (rotation: float32) (geo : Geometry) : Geometry =
        let mtrx = Matrix3x2.Rotation (rotation / (2.0f*float32 Math.PI))
        new TransformedGeometry(factory, geo, mtrx |> matrixToRaw) :> Geometry
      transformer d2DFactory 

    let group = grouper d2DFactory 
    let baz = getThings emptyGeo group 

    let fish = fishGeo :> Geometry 
    let b =  { a = Vector(0.0f, 0.0f); 
               b = Vector(1000.0f, 000.0f);
               c = Vector(00.0f, 1000.0f)}
//    let b =  { a = Vector(300.0f, 300.0f); 
//               b = Vector(1000.0f, 100.0f);
//               c = Vector(-100.0f, 1000.0f)}
    let f = fun i -> fun (box:Box) -> transform box (rotateStep i fish) 
(*    let pic : Geometry array = [|0 .. 360 |] 
                                   |> Array.map (fun i -> 
                                        b |> baz.squareLimit 4 (f (float32 i/10.0f)))
                                        *)
    let rectBrush = new SolidColorBrush(d2DRenderTarget, Interop.RawColor4(0.0f, 0.0f, 0.0f, 0.10f));
    let rect: Interop.RawRectangleF = Interop.RawRectangleF(0.0f, 0.0f, float32 ScreenRes.x_max, float32 ScreenRes.y_max)

    let mutable i = 0
    let printLines (lines:seq<Line option*Turtle>) = 
        let printLine (l: Line) = 
            let ((x1,y1),(x2,y2)) = l 
            d2DRenderTarget.DrawLine(Interop.RawVector2(x1,y1),Interop.RawVector2(x2,y2), pinkBrush,0.9f) 
        for line,_ in lines do
            match line with 
                | None -> ()
                | Some(l) -> printLine l 

    RenderLoop.Run(form, fun _ ->
            d2DRenderTarget.BeginDraw()
            d2DRenderTarget.Clear(new Nullable<Interop.RawColor4>(Interop.RawColor4(0.0f, 0.0f, 0.0f, 0.010f)))
//            draw pic.[x] 
//            simpleTurtle (float x) (0.0<Radians>, (float 500.0,float 500.0)) |> printLines
            turtlePoly midi_16 (0.0<Radians>, (float 500.0,float 500.0)) |> printLines
            inspi midi_17 midi_18 midi_19 0 (0.0<Radians>, (float 500.0,float 500.0)) |> printLines

//            d2DRenderTarget.FillRectangle(rect, rectBrush);
            d2DRenderTarget.Transform <- skew (float32 midi_19/100.0f) (float32 midi_23/100.0f) |> matrixToRaw
            d2DRenderTarget.EndDraw()
            (!swapChain).Present(0, PresentFlags.None) |> ignore
//            Console.ReadLine() |> ignore
        )
    printfn "%A" argv
    backBuffer.Dispose()
    device.ClearState()
    device.Flush()
    device.Dispose()
    (!swapChain).Dispose()
    0 // return an integer exit code