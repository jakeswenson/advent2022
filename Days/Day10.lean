import Days
import Days.Common
import Std
import Lean.Data.Parsec

open Days
open Days.Common
namespace Days.Day10
def day: ProblemNumber := 10

def smallExample := "noop
addx 3
addx -5"

def largerExample := "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"

open Std (RBSet)
open Lean.Parsec
open Lean (Parsec)

inductive Instruction where
  | noop: Instruction
  | addx: Int -> Instruction
  deriving Repr, Ord, Hashable, BEq, DecidableEq, Inhabited

def Instruction.parse: Parsec Instruction := do
  match ← (pstring "noop" <|> pstring "addx") with
  | "noop" => return .noop
  | "addx" => return .addx ((← ws *> many1Chars (pchar '-' <|> digit)) |> String.toInt!)
  | i => fail s!"unknown instruction: {i}"

def Instruction.parseMany [ToString α] (input: α) : Except String $ Array Instruction :=
  match parseInstructions (toString input).iter with 
  | .success _ res => .ok res
  | .error _ msg => .error msg
  where
    parseInstructions: Parsec $ Array Instruction := many (ws *> Instruction.parse <* ws)

#eval Instruction.parseMany smallExample
--theorem verify_parse : Instruction.parseMany smallExample = .ok #[.noop, .addx 3, .addx $ -5] := by simp

structure Registers where
  x: Int
  deriving Repr, BEq, DecidableEq

instance : Inhabited Registers where
  default := ⟨ 1 ⟩

abbrev Cycle := Nat

def run (instructions: Array Instruction): List $ Cycle × Registers :=
  process $ instructions
  where 
    init: Cycle × Registers := (1, default)
    process (insts: Array Instruction) : List $ Cycle × Registers :=
    insts.foldl (init:=[init]) (λ 
    | [], _ => [init]
    | (cycle, registers)::rest, .noop => (cycle + 1, registers)::(cycle, registers)::rest
    | (cycle, r)::rest, .addx x₂ => 
      (cycle + 2, {x:= r.x + x₂})::
      (cycle + 1, r)::
      (cycle, r)::rest)

def parseAndRun! [ToString α] (input: α) : List $ Cycle × Registers :=
  List.reverse <| match Instruction.parseMany input with 
  | .ok insts => run insts
  | .error msg => panic! s!"Error: {msg}"


#eval parseAndRun! smallExample

/--
boo
-/
def part₁ (input: Input) : String := 
  parseAndRun! input
  |>.filter (λ 
    | (20, _) => true
    | (c, _) => if c > 20 ∧ (c - 20) % 40 = 0 then true else false) 
  |>.map (λ | (c, r) => c * r.x)
  |> sum
  |> toString

/--
-/
def part₂ (input: Input) : String :=
  let crt := Array.mkArray 6 (mkArray 40 ".")
  parseAndRun! input
  |>.map (λ 
    | (cycle, {x}) => 
      let char := if (((cycle-1)%40) - x).natAbs ≤ 1
        then "#" 
        else "."
      (cycle, char)
  )
  |>.foldl (init:=crt) (λ 
    | crt, (cycle, char) => 
      let rowNum := ((cycle / 40) % 6)
      let row := crt.get! rowNum
      row.set! (cycle % 40) char
      |> crt.set! rowNum
  )
  |>.map (String.intercalate "" ∘ Array.toList)
  |>.toList
  |> String.intercalate "\n"
  |> ("\n" ++ ·)


def solution : Problem String := ⟨ day, part₁, part₂ ⟩ 


#eval parseAndRun! largerExample
  |>.map (λ 
    | (cycle, {x}) => 
      let char := if (((cycle-1)%40) - x).natAbs ≤ 1
        then "#" 
        else "."
      (cycle, char, x)
  )


#eval testPart₁ solution largerExample (expect:="13140")

#eval testPart₂ solution largerExample (expect:="
..#..##..##..##..##..##..##..##..##..##.
.###...###...###...###...###...###...###
.####....####....####....####....####...
.#####.....#####.....#####.....#####....
.######......######......######......###
########.......#######.......#######....") ()

/-
Solution for day 10 part 2
-/
example : String := "
..###...##..##..####.###...##..#....#..#
.#.......#.#..#.#....#..#.#..#.#....#..#
.###.....#.#....###..#..#.#....#....####
.#.......#.#....#....###..#.##.#....#..#
.#....#..#.#..#.#....#....#..#.#....#..#
.####..##...##..#....#.....###.####.#..#
"