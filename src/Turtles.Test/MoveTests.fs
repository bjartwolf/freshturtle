module Turtles.Test

open FsCheck
open FsCheck.Xunit
open System 
open My.Turtles

[<Property>]
let ``Moving 360n + 180 should move in opposite direction`` (n: int) =
    let t0 = (1.0<Radians>, (0.0, 0.0)): Turtle
    let t1 = t0 
                |> move 10.0 
    let t2 = t0 |> turn (double n*2.0*Math.PI*1.0<Radians>)
                |> turn (Math.PI*1.0<Radians>)
                |> move 10.0 
    let _,(x,y) = t2
    let t3 = 0.0<Radians>,(-x,-y)
    isSamePosition'ish 10 t1 t3

[<Property>]
let ``Given any location, heading 0 moving 90 degrees and forward 4 times end up in same location`` (x_dec: decimal) (y_dec: decimal) = 
    let x = double x_dec
    let y = double y_dec
    let y' = max -1e8 (min 1e8 y)
    let x' = max -1e8 (min 1e8 x)
    let t1: Turtle = 0.0<Radians>, (x', y')
    let t2 = t1 |> move 10.0 |> turn90
                |> move 10.0 |> turn90
                |> move 10.0 |> turn90
                |> move 10.0 
    isSamePosition'ish 5 t1 t2

[<Property>]
let ``Given any location and heading moving 90 degrees and forward 4 times end up in same location to three digits`` (x_dec: decimal) (y_dec: decimal) (heading_int: int)= 
    let x = double x_dec
    let y = double y_dec
    let y' = max -1e8 (min 1e8 y)
    let x' = max -1e8 (min 1e8 x)
    let heading = double (heading_int/10000) *1.0<Radians>
    let t: Turtle = heading, (double (x'), double (y'))
    let t1 = t |> move 0.0 // Just to get the rounding of the move function
    let t2 = t |> move 10.0 |> turn90
               |> move 10.0 |> turn90
               |> move 10.0 |> turn90
               |> move 10.0 
    isSamePosition'ish 7 t1 t2


[<Property>]
let ``Given any location and heading moving 60 degrees and forward 6 times end up in same location to three digits`` (x_dec: decimal) (y_dec: decimal) (heading_int: int)= 
    let x = double x_dec
    let y = double y_dec
    let y' = max -1e8 (min 1e8 y)
    let x' = max -1e8 (min 1e8 x)
    let heading = float (heading_int/10000) *1.0<Radians>
    let t: Turtle = heading, (double (x'), double (y'))
    let t1 = t |> move 0.0 // Just to get the rounding of the move function
    let t2 = t |> move 10.0 |> turn60
               |> move 10.0 |> turn60
               |> move 10.0 |> turn60
               |> move 10.0 |> turn60
               |> move 10.0 |> turn60
               |> move 10.0 
    isSamePosition'ish 7 t1 t2

[<Property>]
let ``Turning, moving, turning double the other way and then moving should leave y zero`` (heading: decimal)= 
    let heading = double heading * 1.0<Radians> 
    let t1: Turtle = 0.0<Radians>, (0.0,0.0)
    let t2 = t1 |> turn heading 
                |> move 10.0
                |> turn (-2.0*heading)
                |> move 10.0
    let _,(_,y) = t2
    roundN 5 y = 0.0

[<Property>]
let ``Turning, moving, turning the other way and then moving then turning then moving should leave y zero`` (heading: decimal)= 
    let heading = double heading * 1.0<Radians>
    let t1: Turtle = 0.0<Radians>, (0.0,0.0)
    let t2 = t1 |> turn heading 
                |> move 10.0
                |> turn (-heading)
                |> move 10.0
                |> turn (-heading)
                |> move 10.0
    let _,(_,y) = t2
    roundN 10 y = 0.0

[<Property>]
let ``Moving a multiple of 360 should move forward the same`` (n: int) =
    let t0: Turtle = (1.0<Radians>, (0.0, 0.0)) 
    let t1: Turtle = t0 
                        |> move 10.0 
    let t2: Turtle = t0 
                        |> turn ((double n)*2.0<Radians>*Math.PI)
                        |> move 10.0
    isSamePosition'ish 3 t1 t2
(*

[<Property>]
let ``A simple turtle should turn around no matter what the turning angle is`` (turning: int) =
    let t0: Turtle = (0.0<Radians>, (0.0, 0.0)) 
    let _, t1 = Seq.last (simpleTurtle turning t0)
    isSamePosition'ish 3 t0 t1
*)