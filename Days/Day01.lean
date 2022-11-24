import Days

namespace Days.Day01
open Days

def part1 (input: Input) : Int := 
  countIncreases none nums
  where 
    nums := List.map (String.toInt!) input.lines
    countIncreases (last: Option Int) (nums: List Int): Nat := 
      match last, nums with 
      | _, [] => 0
      | none, x::xs => countIncreases (some x) xs
      | some last, x::xs => countIncreases (some x) xs + if x > last then 1 else 0


def solution := Problem.define 1 part1

#eval 7 == Problem.test solution "199
200
208
210
200
207
240
269
260
263"