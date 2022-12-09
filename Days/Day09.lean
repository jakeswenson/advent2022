import Days
import Days.Common
import Std
import Lean.Data.Parsec

open Days
open Days.Common
namespace Days.Day09
def day: ProblemNumber := 09

open Std (RBSet mkRBSet)
open Lean.Parsec
open Lean (Parsec)

def sample := "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"

structure Point where
  x: Int
  y: Int
  deriving Ord, BEq, DecidableEq, Hashable, Inhabited

abbrev PointSet: Type := RBSet Point compare

instance : ToString Point where
  toString point := s!"({point.x},{point.y})"

instance : Repr Point where
  reprPrec point _ := 
    .text (toString point)

structure Rope where
  head: Point
  tail: Array Point

instance : Inhabited Rope where
  default := { head := default, tail := #[default]}

instance : Repr Rope where
  reprPrec rope _ := 
    .text s!"h{rope.head}{rope.tail}"

def toArray (range: Std.Range) : Array Nat := Id.run <| do
  let mut result := #[]

  for i in range do
    result := result.push i

  return result

def Rope.visualString (rope: Rope) : String := Id.run <| do
  let points := rope.head::rope.tail.toList
  let maxX: Int := points|>.map (·.x) |>.maximum? |>.get!
  let minX: Int := points|>.map (·.x) |>.minimum? |>.get!
  let maxY: Int := points|>.map (·.y) |>.maximum? |>.get!
  let minY: Int := points|>.map (·.y) |>.minimum? |>.get!
  if minY < 0 ∨ minX < 0 then panic! "Need to adjust min i guess"
  
  let gridSizeX := maxX + 1 |>.natAbs
  let gridSizeY := maxY + 1 |>.natAbs

  let squareGrid := Max.max gridSizeX gridSizeY

  let mut grid: Array $ Array String := Array.mkArray squareGrid (Array.mkArray squareGrid "·")

  for (idx, point) in points.enum do
    let yRow: Array String := grid.get! point.y.natAbs
    let char: String := 
      if idx = 0 then "H" else 
      if idx < 10 then toString idx
      else "X"
    let result := yRow.set! point.x.natAbs char
    let newGrid := grid.set! point.y.natAbs result
    grid := newGrid
  
  let p: Array String :=
    grid.reverse 
    |>.map Array.toList 
    |>.map (String.intercalate "")

  return p
  |>.toList
  |> (String.intercalate "\n")
  |>.push '\n'
  |>.append (Array.mkArray squareGrid "-" |>.toList |> String.intercalate "")
  |>.push '\n'
  |>.append ([:Min.min squareGrid 10] |> toArray |>.map toString |>.toList |> String.intercalate ""
  |>.append s!"\n {squareGrid}×{squareGrid}")

def Rope.print (rope: Rope) : IO Unit := do
  IO.println rope.visualString
  IO.println ""

inductive Direction where
  | right : Direction 
  | up : Direction
  | down : Direction
  | left : Direction
  deriving Repr, Inhabited

inductive RopeMovement where
  | move : Direction -> Nat -> RopeMovement
  deriving Repr, Inhabited

def RopeMovement.parse : Parsec RopeMovement := do
  match ← (pchar 'R' <|> pchar 'L' <|> pchar 'U' <|> pchar 'D') <* ws, ← many1Chars digit with
  | 'R', cnt => return .move .right cnt.toNat!
  | 'D', cnt => return .move .down cnt.toNat!
  | 'U', cnt => return .move .up cnt.toNat!
  | 'L', cnt => return .move .left cnt.toNat!
  | m, _ => fail s!"Failed to parse rope movement from char '{m}'"

def RopeMovement.parseMovements [ToInput α] (i: α) : Except String (Array RopeMovement) :=
  toInput i
  |>.lines
  |>.map (·.trim.text.iter)
  |>.map RopeMovement.parse
  |>.foldl (init:=.ok #[]) λ
  | .error msg, _ | _, .error _ msg => .error msg
  | .ok state, .success _ mvmt => .ok <| state.push mvmt

#eval RopeMovement.parseMovements sample

def pointΔ (leader: Point) (follower: Point) : Point :=
  match follower, leader with
  | { x:=tx, y:=ty }, { x:=hx, y:=hy } => 
    let Δx := hx-tx 
    let Δy := hy-ty
    { x:= Δx, y:=Δy }

instance : Sub Point where
  sub := pointΔ

def Point.asUnitVec (point: Point) : Point :=
  let {x, y} := point
  { x:= if x = 0 then 0 else x/x.natAbs,
    y:= if y = 0 then 0 else y/y.natAbs }

instance : Add Point where
  add p₁ p₂ := { x := p₁.x + p₂.x, y := p₁.y + p₂.y }

def adjustKnot (leader: Point) (follower: Point) : Point :=
  let delta@{ x:= Δx, y:=Δy } := leader - follower 
  if Δx.natAbs ≥ 2 ∨ Δy.natAbs ≥ 2 
  then follower + delta.asUnitVec
  else follower

def adjustTailKnots (rope: Rope): Rope :=
  { rope with tail := foldAdjustKnots rope.head rope.tail }
  where 
    foldAdjustKnots (head: Point) (knots: Array Point): Array Point :=
      knots.foldl (init:=(head, #[])) (λ
      | (leader, tail), knot =>
        let newPos := adjustKnot leader knot
        (newPos, tail.push newPos))
      |>.snd

/-
...H.. (3,4)
....T. (4,3)
......
......
s.....

..HT.. (2,4)(3,4)
......
......
......
s.....
-/
#eval Rope.mk (.mk 3 4) #[.mk 4 3] |> Rope.print

#eval adjustKnot (.mk 2 4) (.mk 4 3)
#eval adjustTailKnots { head := .mk 2 4, tail := #[.mk 4 3] }

#eval Rope.print <| adjustTailKnots { head := .mk 2 4, tail := #[.mk 4 3] }

/-
..T...(2,4)
...H..(3,3)
......
......
s.....

......
...TH.(3,3)(4,3)
......
......
s.....

......
....TH(4,3),(5,3)
......
......
s.....
-/
#eval adjustTailKnots { head := .mk 3 3, tail := #[.mk 2 4] }
#eval adjustTailKnots { head := .mk 5 3, tail := #[.mk 3 4] }

def Rope.move (rope: Rope) (mvmts: Array RopeMovement) : Array Rope :=
  rope
  |> applyMovements
  where
    mvHead (dir: Direction) (rope: Rope): Rope :=
      match dir with
      | .up => { rope with head := { rope.head with y := rope.head.y + 1}}
      | .down => { rope with head := { rope.head with y := rope.head.y - 1}}
      | .left => { rope with head := { rope.head with x := rope.head.x - 1}}
      | .right => { rope with head := { rope.head with x := rope.head.x + 1}}

    apply (rope: Rope) (result: Array Rope) (mvmt: RopeMovement) : Rope × Array Rope :=
    match mvmt with 
    | .move _ 0 => (rope, result)
    | .move dir (cnt + 1) => 
      let newRopeHead := mvHead dir rope
      let newRope := adjustTailKnots newRopeHead
      apply newRope (result.push newRope) (.move dir cnt) 
    
    applyMovements (rope: Rope) : Array Rope :=
      mvmts.foldl (init:=(rope, #[rope])) (λ
        | (rope, list), mvmt => apply rope list mvmt) |>.snd

def printRopes (ropes: Array Rope) : IO Unit := do
  for rope in ropes do
    rope.print
  return ()

#eval Rope.move (default) <| (RopeMovement.parseMovements sample).toOption.get!

#eval printRopes <| Rope.move (default) <| (RopeMovement.parseMovements sample).toOption.get!

def Rope.tailPath (rope: Rope) (mvmts: Array RopeMovement) : Array Point :=
  rope.move mvmts
  |>.map (·.tail)
  |>.map (λ knots => knots.reverse.toList.head!)

/-
..##..
...##.
.####.
....#.
s###..
-/
#eval Rope.tailPath (default) <| (RopeMovement.parseMovements sample).toOption.get!

def Rope.distinctTailPath (rope: Rope) (mvmts: Array RopeMovement) : PointSet :=
  rope.tailPath mvmts
  |> RBSet.ofArray (cmp:=compare)


#eval Rope.distinctTailPath (default) <| (RopeMovement.parseMovements sample).toOption.get!

def part₁ (input: Input) : Nat := 
  mvmts
  |> Rope.distinctTailPath (rope:=default)
  |>.size
  where
    mvmts: Array RopeMovement := 
      match RopeMovement.parseMovements input with
      | .ok res => res 
      | .error error => panic! s!"Error: {error}"

/--
-/
def part₂ (input: Input) : Nat :=
  mvmts
  |> Rope.distinctTailPath (rope:=.mk default (Array.mkArray 9 default))
  |>.size
  where
    mvmts: Array RopeMovement := 
      match RopeMovement.parseMovements input with
      | .ok res => res 
      | .error error => panic! s!"Error: {error}"

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 

#eval testPart₁ solution sample (expect:=13)


-- #eval printRopes <| Rope.move (rope:=.mk default (Array.mkArray 9 default)) <| (RopeMovement.parseMovements sample).toOption.get!
#eval testPart₂ solution sample (expect:=1)

def sample₂ := "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

#eval printRopes <| Rope.move (rope:=.mk default (Array.mkArray 9 default)) <| (RopeMovement.parseMovements sample₂).toOption.get!
#eval testPart₂ solution sample₂ (expect:=36)


