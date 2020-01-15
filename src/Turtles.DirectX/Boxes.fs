module Boxes 
open SharpDX
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Direct2D1

type RawVector = Interop.RawVector2
type Vector = Vector2 

type Box = 
  { a : Vector 
    b : Vector
    c : Vector }

type Picture = Box -> Geometry

let turn box = 
  { a = box.a + box.b
    b = box.c 
    c = Vector2.Negate box.b }

let flip box = 
  { a = box.a + box.b
    b = Vector2.Negate box.b
    c = box.c }

let toss box = 
  { a = box.a + ((box.b + box.c) * 0.5f)
    b = (box.b + box.c) * 0.5f 
    c = (box.c - box.b) * 0.5f }

let scaleHorizontally (s : float32) { a = a; b = b; c = c } =
  { a = a
    b = b * s
    c = c } 

let translate (translate: Vector2) (box: Box) = 
    { box with a = box.a + translate }

let scaleVertically (s: float32) { a = a; b = b; c = c } = 
  { a = a
    b = b 
    c = c * s }

let moveHorizontally offset { a = a; b = b; c = c } = 
  { a = a + (b * offset)
    b = b
    c = c }
  
let moveVertically (offset: float32) { a = a; b = b; c = c } = 
  { a = a + (offset * c)
    b = b
    c = c }

let splitHorizontally f box =
  let top = box |> moveVertically (1.0f - f) |> scaleVertically f  
  let bottom = box |> scaleVertically (1.0f - f)
  (top, bottom)

let splitVertically f box = 
  let left = box |> scaleHorizontally f
  let right = box |> moveHorizontally f |> scaleHorizontally (1.0f - f)
  (left, right)