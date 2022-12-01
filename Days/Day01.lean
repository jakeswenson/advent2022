import Days

namespace Days.Day01
open Days

structure Elf :=
  carrying: List Int

def part1 (input: Input) : Int := 
  elfCarryingTheMost.get!
  where 
    elves: List Elf := input.text.splitOn "\n\n" 
        |> List.map (fun x => x.splitOn "\n" |> List.map String.toInt!)
        |> List.map Elf.mk
    countCarrying (elf : Elf) : Int := 
      elf.carrying
      |> List.foldl (init:=0) (·+·)

    elfCarryingTheMost := 
      List.map countCarrying elves
      |> List.maximum?

def part2 (input: Input) : Int := 
  top3Total
  where 
    elves: List Elf := input.text.splitOn "\n\n" 
        |> List.map (fun x => x.splitOn "\n" |> List.map String.toInt!)
        |> List.map Elf.mk

    countCarrying (elf : Elf) : Int := 
      elf.carrying
      |> List.foldl (init:=0) (·+·)

    top3Elves : Option $ Int × Int × Int := 
      List.map countCarrying elves
      |> List.foldl (init := none) (fun 
      | none, elf => (elf, elf, elf)
      | some (top₁, top₂, top₃), elf => 
        if elf > top₁ then (elf, top₁, top₂)
        else if elf > top₂ then (top₁, elf, top₂)
        else if elf > top₃ then (top₁, top₂, elf)
        else (top₁, top₂, top₃)
      )
    top3Total := let (top₁, top₂, top₃) := top3Elves.get!; top₁ + top₂ + top₃


def solution := Problem.define 1 part1 part2

def sample := "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"


#eval 24000 == Problem.testPart₁ solution sample
#eval 45000 == Problem.testPart₂ solution sample