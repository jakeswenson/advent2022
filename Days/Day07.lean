import Days
import Days.Common
import Std
import Lean.Data.Parsec

open Days
open Days.Common
namespace Days.Day07
def day: ProblemNumber := 07

open Std (RBSet)

/--
-/
def part₁ (input: Input) : Nat := 
  0

/--
-/
def part₂ (input: Input) : Nat :=
  0 

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 

def sample := ""

#eval testPart₁ solution sample (expect:=7)
#eval testPart₂ solution sample (expect:=19)

