(*** hide ***)
module My.Turtles
open System
//open Operators

(** 
# Helper functions...
We introduce some simple helper functions, but please skip to Turtles if you are easiliy bored.
*)

(** 
Before we even introduce the turtles, we need angles in radians
and degrees and a conversion between them...
*)
[<Measure>] type Radians 
[<Measure>] type Degrees 
let radiansPerDegree = Math.PI/180.0*1.0<Radians/Degrees>
let convertRad2Deg x = x / radiansPerDegree 
let convertDeg2Rad x = x * radiansPerDegree 

(** 
The turtle is using floats, so we must round of to a certain number of digits
at times to for example see that we are in approximately the same position as we started.
*)
let roundN (nrOfdoubles: int) (value:double) = Math.Round(value, nrOfdoubles)
let round5 = roundN 5 
let round10 = roundN 10 

(** 
# The definition of a Turtle 
A Turtle has a direction and a position in ℝ²
It does not know about the world around it, which it what
separates it from a turmite, such as Langton's ant.
Its internal direction is in radians.
*)
type Dir = float<Radians> 
type Position = double*double
type Turtle = Dir * Position


(** 
## Moving and turning 
A Turtle can move a certain length in the direction it is facing
The result of a move is a new turtle in a new position, with the same direction 
*)
type Length = double
let move (l:Length)(t: Turtle) : Turtle = 
    let dir,(x,y) = t 
    let x' = x + l * cos (dir / 1.0<Radians>)
    let y' = y + l * sin (dir / 1.0<Radians>)
    let pos' = (x',y')
    (dir, pos')

(** 
Turning is always to the left with positive numbers, to turn right
use negative numbers. Because mathematics.
As with move, we return a new Turtle
*)
let turn (a:float<Radians>) (t: Turtle) : Turtle = 
    let dir, pos = t 
    let dir' = dir + a
    (dir', pos) 

let turnDeg (a:float<Degrees>) (t: Turtle) : Turtle = 
    turn (convertDeg2Rad a ) t

let turn60 = turnDeg 60.0<Degrees>
let turn90 = turnDeg 90.0<Degrees>

(** 
It can be useful to compare turtle positions by creating a new turtle in a rounded position
*)
let round (digits:int) (t:Turtle) : Turtle =
    let dir,(x,y) = t
    let rounder = roundN digits
    let pos' = (rounder x,rounder y)
    (dir, pos')

(** 
Comparing two turtles to see if they are in approximately the same position to the accuracy given by digits 
*)
let isSamePosition'ish (digits:int) (t1: Turtle) (t2: Turtle): bool =
    let t1' = round digits t1
    let t2' = round digits t2
    let _, (x1,y1) = t1'
    let _, (x2,y2) = t2'
    x1 = x2 && y1 = y2


(** 
The shortest path between two turtles is a line.
We use singles for co-ordinates, because they are to be drawn by
a graphics library that doesn't care for doubles anyway
*)
type Line = (single*single)*(single*single)
type Lines = Line list

(** 
A line is really just the position of two turtles
*)
let turtleLine (t:Turtle) (t': Turtle) : Line = let _,(x,y) = t
                                                let _,(x',y') = t'
                                                let l: Line = ((single x,single y),(single x',single y'))
                                                l

(** 
Returns true if the turtle's direction is close to a multiple of two pi
*)
let closeToPi (dir:Dir)  = 
    let distFromPi dir = Math.Abs(float dir) % (2.0*Math.PI) 
    let closeToPi = distFromPi dir 
    closeToPi < 0.0000000001 || closeToPi > (2.0*Math.PI-0.0000000001)

(** 
A Turtle-sequencee is a sequence of turtles and the lines it produces while moving
*)
let rec simpleTurtle (turning: float) (t: Turtle): seq<Line option*Turtle> = 
    let edges = 360.0 / (float turning)
    let π = Math.PI
    let step = move (600.0 / float edges) 
    let degreesToTurn  = float turning * 1.0<Degrees>
    seq {
         let t' = step t |> turnDeg degreesToTurn  
         yield (Some (turtleLine t t'), t')
         let dir, _= t'
         if closeToPi dir then 
             yield (None, t')
         else 
             yield! (simpleTurtle turning t')
    }

let rec inspi (stepLength: int) (angle: int) (inc: int) (currentDegs: int) (t: Turtle): seq<Line option*Turtle> = 
   let step = move (float stepLength)
   let degreesToTurn  = float angle * -1.0<Degrees>
   seq {
        let t' = step t |> turnDeg degreesToTurn 
        yield (Some (turtleLine t t'), t')
        let dir, _ = t'
        if (currentDegs + angle) % 360 = 0 then
            yield (None, t')
        else 
            yield! (inspi stepLength (angle+inc) inc (currentDegs+angle) t')
    }

let rec turtlePoly (turning: int) (t: Turtle): seq<Line option*Turtle> = 
   let edges = 360.0 / (float turning)
   let step = move (1000.0 / edges)
   let degreesToTurn  = float turning * 1.0<Degrees>
   seq {
        let t' = step t |> turnDeg degreesToTurn 
        yield (Some (turtleLine t t'), t')
        let t'' = step t' |> turnDeg (2.0*degreesToTurn)
        yield (Some (turtleLine t' t''), t')
        let dir, _ = t''
        if closeToPi dir then 
            yield (None, t'')
        else 
            yield! (turtlePoly turning t'')
    }