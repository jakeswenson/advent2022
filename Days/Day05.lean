import Days
import Days.Common
import Std
import Lean.Data.Parsec

namespace Days.Day05
open Days
open Common
open Std (RBMap)

open Lean.Parsec
open Lean (Parsec)

/--
The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.

The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.

The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.

They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.

Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:

[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 
In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:

        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3
Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:

        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3
Finally, one crate is moved from stack 1 to stack 2:

        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3
The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.

After the rearrangement procedure completes, what crate ends up on top of each stack?
-/
structure Stacks where
  stacks: RBMap Nat (List Char) compare
  deriving Repr

def stackItem : Parsec $ Option Char := do
  _ ← pchar '['
  let c ← asciiLetter
  _ ← pchar ']'
  if (← peek?) = ' '
  then skip
  return some c

#eval stackItem "[A]".iter

def Stacks.parseLine: Parsec $ List $ Option Char :=
  parse
  where 
    noItem : Parsec $ Option Char := do
      _ ← pstring "   "
      if (← peek?) = some ' '
      then skip
      return none

    parse : Parsec $ List $ Option Char := do 
      let array ← many (stackItem <|> noItem)
      return array.toList

#eval Stacks.parseLine "        [Z]".iter

def Stacks.parseLines (i: Input) : List $ List $ Option Char := 
  i.lines
  |>.map (·.iter)
  |>.map Stacks.parseLine
  |>.filterMap (λ
  | .success _ res => some res
  | .error _ _ => none)


#eval Input.mk "        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3" |> Stacks.parseLines

def Stacks.parse (input: Input) : Stacks :=
  input
  |> Stacks.parseLines
  |>.map (λ l =>
    l.enumFrom 1
    |>.foldl (init:=Std.mkRBMap Nat (List Char) compare) (λ (state: RBMap Nat (List Char) compare) (pair: Nat × Option Char) => 
      state.insert pair.fst (match pair.snd with 
      | some c => [c]
      | none => [])
    )
  )
  |>.foldl (init:=Stacks.mk (Std.mkRBMap Nat (List Char) compare)) (λ (stacks: Stacks) (map: RBMap Nat (List Char) compare) =>
    stacks.stacks.mergeWith (λ _ l₁ l₂ => l₁ ++ l₂)  map
    |> Stacks.mk
  )

#eval Input.mk "        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3" |> Stacks.parse

structure Operation where
  count: Nat
  fromStack: Nat
  toStack: Nat
  deriving Repr, DecidableEq, Inhabited

def problemSeperator := "\n\n"

def toOp (countStr fromStackStr toStackStr: String) : Option Operation := do 
  let count ← String.toNat? countStr
  let fromStack ← String.toNat? fromStackStr
  let toStack ← String.toNat? toStackStr
  return Operation.mk count fromStack toStack

-- move 1 from 2 to 1
def opParser := do
  let _ ← pstring "move "
  let countStr ← many1Chars digit
  let _ ← pstring " from "
  let fromStackStr ← many1Chars digit
  let _ ← pstring " to "
  let toStackStr ← many1Chars digit
  return toOp countStr fromStackStr toStackStr 

#eval opParser "move 1 from 2 to 1".iter

def Operation.parse [ToString α] (i: α) : Option Operation :=
  match opParser (ToString.toString i).iter with
  | .success _ res => res
  | .error _ _ => none

#eval Operation.parse "move 1 from 2 to 1"

def Operation.parseMany (i: Input) : List Operation :=
  i.lines 
  |>.map Operation.parse
  |>.filterMap id

#eval Operation.parseMany ⟨ "move 1 from 2 to 1
move 1 from 2 to 3" ⟩ = [Operation.mk 1 2 1, Operation.mk 1 2 3]

def stacker (mover: List Char -> List Char) (input: Input): Stacks :=
  runOps
  where
    problemParts := input.splitOn problemSeperator
    startingStacksText := problemParts.get! 0
    operationsText := problemParts.get! 1
    stacks : Stacks := Stacks.parse startingStacksText 
    ops : List Operation := Operation.parseMany operationsText
    runOps := 
      ops.foldl (init:=stacks) (λ 
      | state, Operation.mk count fromStack toStack => 
        let source: List Char := state.stacks.find? fromStack |>.get!
        let target: List Char := state.stacks.find? toStack |>.get!
        let newSource := source.drop count 
        let moved := source.take count |> mover
        let newTarget: List Char := moved.foldr (init:=target) (λ (el: Char) t => el::t)
        state.stacks.insert fromStack newSource 
        |>.insert toStack newTarget
        |> Stacks.mk
      )

def stacker₉₀₀₀ : Input → Stacks := stacker (·.reverse)

#eval "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1" |> Input.mk |> stacker₉₀₀₀

#eval "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3" |> Input.mk |> stacker₉₀₀₀

def part₁ (input: Input) : String := 
  stacker₉₀₀₀ input
  |>.stacks
  |>.valuesList
  |>.map List.head!
  |> String.mk 

/--
As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.

Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.

The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup holder, and the ability to pick up and move multiple crates at once.

Again considering the example above, the crates begin in the same configuration:

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 
Moving a single crate from stack 2 to stack 1 behaves the same as before:

[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 
However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates stay in the same order, resulting in this new configuration:

        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3
Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:

        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3
Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate C that gets moved:

        [D]
        [N]
        [Z]
[M] [C] [P]
 1   2   3
In this example, the CrateMover 9001 has put the crates in a totally different order: MCD.

Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be ready to unload the final supplies. After the rearrangement procedure completes, what crate ends up on top of each stack?
-/
def stacker₉₀₀₁ : Input → Stacks := stacker id

def part₂ (input: Input) : String :=
  stacker₉₀₀₁ input
  |>.stacks
  |>.valuesList
  |>.map List.head!
  |> String.mk 

def solution : Problem String := ⟨ 5, part₁, part₂ ⟩ 

def sample := "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"


def sampleInput := Input.mk sample

#eval stacker₉₀₀₀ sampleInput

#eval testPart₁ solution sample (expect:="CMZ")
#eval testPart₂ solution sample (expect:="MCD")
