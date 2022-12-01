namespace Days

structure Input := 
  text: String 

structure ProblemNumber : Type where
  of ::
  day: Nat 
  deriving Repr, Ord, DecidableEq

instance : Inhabited ProblemNumber where
  default := ProblemNumber.of 1

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

instance : LT ProblemNumber where
  lt 
  | p₁, p₂ => p₁.day < p₂.day

instance : LE ProblemNumber where
  le
  | p₁, p₂ => p₁.day ≤ p₂.day

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
  | none => panic! "Can't parse problem number"

structure Problem :=
  define ::
  day: ProblemNumber
  part1: Input -> Int
  part2: Option $ Input -> Int

def Problem.padDay (day: ProblemNumber) : String := 
  if day.day < 10 then s!"0{day.day}" else s!"{day.day}"

instance : ToString ProblemNumber where
  toString p := Problem.padDay p

def Problem.run (p:  Problem) (i: Input) : IO Unit := do
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

def Input.lines : (@& Input) → List String
  | ⟨s⟩ => String.splitOn s "\n"


def Problem.testPart₁ (p: Problem) (input: String) : Int := 
  p.part1 (Input.mk input)


def Problem.testPart₂ (p: Problem) (input: String) : Int := 
  p.part2.map runPart2
  |> Option.get!
  where
    runPart2 (f: Input -> Int) := f (Input.mk input)


