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
  tail: Point
  deriving Inhabited

instance : Repr Rope where
  reprPrec rope _ := 
    .text s!"h{rope.head}{rope.tail}"

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
  | .error msg, _ => .error msg
  | _, .error _ msg => .error msg
  | .ok state, .success _ mvmt => .ok <| state.push mvmt

#eval RopeMovement.parseMovements sample


def adjustTail (oldHead: Point) (rope: Rope): Rope :=
  match rope.tail, rope.head with
  | { x:=tx, y:=ty }, { x:=hx, y:=hy } => 
    let taxiDistance := (hx-tx).natAbs + (hy-ty).natAbs
    if taxiDistance ≥ (if hx = tx ∨ hy = ty then 2 else 3) 
    then {rope with tail := oldHead}
    else rope
    -- let Δx := ((hx-tx) + if hy ≠ ty then ((hy-ty)/2).natAbs else 0) /2
    -- let Δy := ((hy-ty) + if hx ≠ tx then ((hx-tx)/2).natAbs else 0) /2
    -- { rope with tail := { x := rope.tail.x + Δx, y := rope.tail.y + Δy } }

-- T:0,0 H:1,2 → T:1,1 : tx+(hx-tx)/2, ty+(hx-tx)/2
-- T:0,0 H:2,0 → T:1,0 : tx+(hx-tx)/2,hx
-- 
/-
t(4,3)h(2,4) → T:3,4
Δx = ((2-4)+(4-3)/2)/2 = (-2 + 0) / 2 = -1
Δy = ((4-3)+(2-4)/2)/2 = 1 + -1 / 2

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
#eval adjustTail (.mk 3 4) { head := .mk 2 4, tail := .mk 4 3 }

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
#eval adjustTail (.mk 2 3) { head := .mk 3 3, tail := .mk 2 4 }
#eval adjustTail (.mk 4 3) { head := .mk 5 3, tail := .mk 3 4 }

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
      let newRope := adjustTail rope.head newRopeHead
      apply newRope (result.push newRope) (.move dir cnt) 
    
    applyMovements (rope: Rope) : Array Rope :=
      mvmts.foldl (init:=(rope, #[rope])) (λ
        | (rope, list), mvmt => apply rope list mvmt) |>.snd

#eval Rope.move (default) <| (RopeMovement.parseMovements sample).toOption.get!

def Rope.tailPath (rope: Rope) (mvmts: Array RopeMovement) : Array Point :=
  rope.move mvmts
  |>.map (·.tail)

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
  0

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 


#eval testPart₁ solution sample (expect:=13)
#eval testPart₂ solution sample (expect:=0)

