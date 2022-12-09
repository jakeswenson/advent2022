import Std
namespace Days

structure Input := 
  text: String
  deriving Repr, DecidableEq, Ord

instance : ToString Input where
  toString := (·.text)

def Input.iter : (@& Input) → String.Iterator
  | ⟨s⟩ => s.iter

def Input.lines : (@& Input) → List Input
  | ⟨s⟩ => String.splitOn s "\n" 
      |> .map Input.mk

def Input.trim : (@& Input) → Input
  | ⟨s⟩ => s.trim |> Input.mk

def Input.toInt? : (@& Input) → Option Int
  | ⟨s⟩ => s.toInt?

def Input.toNat? : (@& Input) → Option Nat
  | ⟨s⟩ => s.toNat?

def Input.intLines (i: Input) : List Int :=
  i.lines |>.map (·.toInt?.get!)

def Input.natLines (i: Input) : List Nat :=
  i.lines |>.map (·.toNat?) |>.filterMap id

def Input.splitOn : (@& Input) → String → List Input
  | ⟨s⟩, sep => String.splitOn s sep
    |> List.map .mk

instance : Inhabited Input where
  default := ⟨ "" ⟩ 

class ToInput (α : Type u) where
  toInput : α → Input

export ToInput (toInput)

instance : ToInput String where
  toInput := Input.mk

instance : ToInput Input where
  toInput := id

/--
A number type that models a problem number for a day. 
Only the litteral 1-25 can be parsed into a number.
-/
structure ProblemNumber : Type where
  of ::
  day: Nat 
  deriving Repr, Ord, DecidableEq, BEq

instance : Inhabited ProblemNumber where
  default := ProblemNumber.of 1

instance : LT ProblemNumber := ltOfOrd
instance : LE ProblemNumber := leOfOrd

example : ProblemNumber := .of 1

instance : OfNat ProblemNumber 1 where ofNat := .of 1
instance : OfNat ProblemNumber 2 where ofNat := .of 2
instance : OfNat ProblemNumber 3 where ofNat := .of 3
instance : OfNat ProblemNumber 4 where ofNat := .of 4
instance : OfNat ProblemNumber 5 where ofNat := .of 5
instance : OfNat ProblemNumber 6 where ofNat := .of 6
instance : OfNat ProblemNumber 7 where ofNat := .of 7
instance : OfNat ProblemNumber 8 where ofNat := .of 8
instance : OfNat ProblemNumber 9 where ofNat := .of 9

instance : OfNat ProblemNumber 10 where ofNat := .of 10
instance : OfNat ProblemNumber 11 where ofNat := .of 11
instance : OfNat ProblemNumber 12 where ofNat := .of 12
instance : OfNat ProblemNumber 13 where ofNat := .of 13
instance : OfNat ProblemNumber 14 where ofNat := .of 14
instance : OfNat ProblemNumber 15 where ofNat := .of 15
instance : OfNat ProblemNumber 16 where ofNat := .of 16
instance : OfNat ProblemNumber 17 where ofNat := .of 17
instance : OfNat ProblemNumber 18 where ofNat := .of 18
instance : OfNat ProblemNumber 19 where ofNat := .of 19

instance : OfNat ProblemNumber 20 where ofNat := .of 20
instance : OfNat ProblemNumber 21 where ofNat := .of 21
instance : OfNat ProblemNumber 22 where ofNat := .of 22
instance : OfNat ProblemNumber 23 where ofNat := .of 23
instance : OfNat ProblemNumber 24 where ofNat := .of 24
instance : OfNat ProblemNumber 25 where ofNat := .of 25

def ProblemNumber.ofNat? (n: Nat) :=
  match n with
  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  | 10 | 11 | 12 | 13 | 14 | 15
  | 16 | 17 | 18 | 19 | 20 
  | 21 | 22 | 23 | 24 | 25  => some $ ProblemNumber.of n
  | _ => none

def ProblemNumber.ofNat! (n: Nat) := 
  match ProblemNumber.ofNat? n with
  | some n => n
  | none => panic! s!"Can't parse problem number from '{n}'; it should be between 1 and 25"

structure Problem (α: Type) [ToString α]:=
  define ::
  day: ProblemNumber
  part1: Input -> α
  part2: Option $ Input -> α

def Problem.wrapString (α: Type) [ToString α] (p: Problem α) : Problem String :=
  Problem.define p.day (toString ∘ p.part1) (p.part2.map (λ p₂ input => p₂ input |> toString))

def Problem.padDay (day: ProblemNumber) : String := 
  if day.day < 10 then s!"0{day.day}" else s!"{day.day}"

instance : ToString ProblemNumber where
  toString p := Problem.padDay p

def Problem.run [ToString α] (p: Problem α) (i: Input) : IO Unit := do
  let result₁ := p.part1 i

  IO.println "==========================="
  IO.println s!"DAY {p.day}"
  IO.println "==========================="
  IO.println s!"Part 1: {result₁}"
  IO.println "==========================="
  match p.part2 with
  | some f => 
    let result₂ := f i
    IO.println s!"Part 2: {result₂}"
  | none => 
    IO.println s!"Part 2: Not done yet"

  return ()

def testPart₁ [ToInput κ] [ToString α] [DecidableEq α] (expect: α) (p: Problem α) (input: κ) : IO α := do
  let result := p.part1 <| toInput input
  if result = expect then
    IO.println "Part1 ✅"
  else 
    IO.println "Part1 ❌"
  return result

def testPart₂ [ToInput κ] [ToString α] [DecidableEq α] (expect: α) (p: Problem α) (input: κ) : IO α := do
  match result with
  | none => 
      IO.println "Part 2 not done yet"
      return expect
  | some result => 
    if result = expect then
      IO.println "Part2 ✅"
    else 
      IO.println "Part2 ❌"
    return result
  where
    runPart2 (f: Input -> α) := f (toInput input)
    result := p.part2.map runPart2
