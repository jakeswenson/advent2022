import Days
import Days.Common
import Std

namespace Days.Day03
open Days
open Common
open Std (RBSet)


def compartments (input: Input) : List (List Char × List Char) := 
  input.lines.map (λ (c: Input) => 
    let mid := c.text.length / 2
    c.text.toList
    |>.enum
    |>.partition (fun (idx, _) => idx < mid)
    |>.map (·.map (·.snd)) (·.map (·.snd))
  )

def duplicates (input: Input): List $ Option Char := 
  compartments input
  |>.map (λ
  | (l₁, l₂) =>
    l₁.findSome? (λ c => l₂.find? (c==·))
  )

def priorityOf (c: Char) : Nat :=
  if c ≥ 'a' && c ≤ 'z' 
  then 1 + c.toNat - 'a'.toNat
  else if c > 'A' then 27 + c.toNat - 'A'.toNat
  else panic! s!"Bad char {c}"

def firstLine: Input := ⟨"vJrwpWtwJgWrhcsFMMfFFhFp"⟩
#eval compartments firstLine |>.map (·.fst)
#eval duplicates firstLine
#eval 16 == priorityOf 'p'
#eval 38 == priorityOf 'L'

/--

One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey. Unfortunately, that Elf didn't quite follow the packing instructions, and so a few items now need to be rearranged.

Each rucksack has two large compartments. All items of a given type are meant to go into exactly one of the two compartments. The Elf that did the packing failed to follow this rule for exactly one item type per rucksack.

The Elves have made a list of all of the items currently in each rucksack (your puzzle input), but they need your help finding the errors. Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different types of items).

The list of items for each rucksack is given as characters all on a single line. A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, while the second half of the characters represent items in the second compartment.

For example, suppose you have the following list of contents from six rucksacks:

vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw

The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which means its first compartment contains the items vJrwpWtwJgWr, while the second compartment contains the items hcsFMMfFFhFp. The only item type that appears in both compartments is lowercase p.
The second rucksack's compartments contain jqHRNqRjqzjGDLGL and rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is uppercase L.
The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only common item type is uppercase P.
The fourth rucksack's compartments only share item type v.
The fifth rucksack's compartments only share item type t.
The sixth rucksack's compartments only share item type s.
To help prioritize item rearrangement, every item type can be converted to a priority:

Lowercase item types a through z have priorities 1 through 26.
Uppercase item types A through Z have priorities 27 through 52.
In the above example, the priority of the item type that appears in both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157.

Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?
-/
def part₁ (input: Input) : Int := 
  sum duplicateItemPriorities
  |> Int.ofNat
  where
    duplicateItemPriorities := 
      duplicates input
      |>.map (·.get!)
      |>.map priorityOf

/--
As you finish identifying the misplaced items, the Elves come to you with another issue.

For safety, the Elves are divided into groups of three. Every Elf carries a badge that identifies their group. For efficiency, within each group of three Elves, the badge is the only item type carried by all three Elves. That is, if a group's badge is item type B, then all three Elves will have item type B somewhere in their rucksack, and at most two of the Elves will be carrying any other item type.

The problem is that someone forgot to put this year's updated authenticity sticker on the badges. All of the badges need to be pulled out of the rucksacks so the new authenticity stickers can be attached.

Additionally, nobody wrote down which item type corresponds to each group's badges. The only way to tell which item type is the right one is by finding the one item type that is common between all three Elves in each group.

Every set of three lines in your list corresponds to a single group, but each group can have a different badge item type. So, in the above example, the first group's rucksacks are the first three lines:

vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
And the second group's rucksacks are the next three lines:

wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
In the first group, the only item type that appears in all three rucksacks is lowercase r; this must be their badges. In the second group, their badge item type must be Z.

Priorities for these items must still be found to organize the sticker attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for the second group. The sum of these is 70.

Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?
-/
structure Group ( α: Type ) where 
  memberItems: List α
  deriving Repr

def toGroupsOf₃ (input: Input) : List $ Group Input :=
  input.lines 
  |>.enum 
  |>.groupBy (λ 
  | (idx, _), (other, _) => idx / 3 == other / 3) 
  |>.map (·.map Prod.snd)
  |>.map Group.mk

def groupMembersToItemSet (group: Group Input): Group $ RBSet Char compare :=
  group.memberItems.map (·.text)
  |>.map (λ (memberItems: String) => 
    memberItems.foldl (init:=∅) (·.insert)
  )
  |> Group.mk

def commonItems (group: Group $ RBSet Char compare): RBSet Char compare :=
  intersectAll
  where
    setInsersect (map map₂: RBSet Char compare) : RBSet Char compare :=
      RBSet.intersectWith compare (λ a _ => a) map map₂
    first := group.memberItems.head!
    rest := group.memberItems.tail!
    intersectAll: RBSet Char compare:=
      rest.foldl (init:=first) setInsersect

def groupCommonElement (group: Group Input): Char :=
  groupMembersToItemSet group
  |> commonItems
  |>.toList
  |> List.head!


def part₂ (input: Input) : Int := 
  input
  |> toGroupsOf₃
  |>.map groupCommonElement
  |>.map priorityOf
  |> sum
  |> Int.ofNat

def solution : Problem := ⟨ 3, part₁, part₂ ⟩ 

def sample := "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

def sampleInput := Input.mk sample

#eval duplicates sampleInput
#eval duplicates sampleInput |>.map (priorityOf ∘ (·.get!))

#eval sampleInput |> toGroupsOf₃
#eval sampleInput |> toGroupsOf₃ |>.map groupCommonElement
#eval Group.mk [⟨"Zk"⟩, ⟨"Z"⟩] |> groupMembersToItemSet
#eval Group.mk [⟨"Z"⟩, ⟨"Z"⟩] |> groupMembersToItemSet |> commonItems
#eval Group.mk [⟨"Z"⟩, ⟨"Z"⟩] |> groupCommonElement

#eval testPart₁ solution sample (expect:=157)
#eval testPart₂ solution sample (expect:=70)