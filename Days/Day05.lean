import Days
import Days.Common
import Std
import Lean.Data.Parsec

namespace Days.Day05
open Days
open Common
open Std (RBSet)

/--
-/
def part₁ (input: Input) : Nat := 
  0

/--
-/
def part₂ (input: Input) : Nat :=
   0

def solution : Problem := ⟨ 5, part₁, part₂ ⟩ 

def sample := ""

def sampleInput := Input.mk sample

#eval testPart₁ solution sample (expect:=0)
#eval testPart₂ solution sample (expect:=0)
