import Days
import Days.Common
import Std
import Lean.Data.Parsec
import Init.Data.Format.Basic

open Days
open Days.Common
namespace Days.Day08
def day: ProblemNumber := 8

open Std (RBSet)

/--
The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a tree house.

First, determine whether there is enough tree cover here to keep a tree house **hidden**. To do this, you need to count the number of trees that are **visible from outside the grid** when looking directly along a row or column.

The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input). For example:

```
30373
25512
65332
33549
35390
```

Each tree is represented as a single digit whose value is its height, where `0` is the shortest and `9` is the tallest.

A tree is **visible** if all of the other trees between it and an edge of the grid are **shorter** than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are **visible** - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the **interior nine trees** to consider:

  - The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
  - The top-middle 5 is visible from the top and right.
  - The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
  - The left-middle 5 is visible, but only from the right.
  - The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
  - The right-middle 3 is visible from the right.
  - In the bottom row, the middle 5 is visible, but the 3 and 4 are not.

With 16 trees visible on the edge and another 5 visible in the interior, a total of **21** trees are visible in this arrangement.

Consider your map; **how many trees are visible from outside the grid?**
-/
def sample := "30373
25512
65332
33549
35390"

structure Tree where
  height: Nat
  deriving Ord, BEq, DecidableEq, Inhabited, Repr

instance : ToString Tree where
  toString t := toString t.height

instance : LT Tree := ltOfOrd
instance : LE Tree := leOfOrd

structure ForestMap where
  trees: Array $ Array Tree
  deriving BEq, DecidableEq, Inhabited

instance : ToString ForestMap where
  toString map := 
    map.trees
    |>.map (λ row => String.intercalate "" (row |>.map toString |>.toList) )
    |>.toList
    |> String.intercalate "\n"

def ForestMap.parse (lines: List String) : ForestMap :=
  lines
  |>.map (λ line =>
    line.data
    |>.map (λ c => c.toNat - '0'.toNat |> Tree.mk)
    |> #[].appendList)
  |> #[].appendList
  |> ForestMap.mk

def sampleMap := sample |> toInput |>.lines |>.map (·.text) |> ForestMap.parse

#eval sampleMap

def ForestMap.rows (map: ForestMap) : Nat :=
  map.trees.size

def ForestMap.columns (map: ForestMap) : Nat :=
  map.trees[0]!.size

def ForestMap.lastColumn (map: ForestMap) : Nat := 
  map.columns - 1

def ForestMap.lastRow (map: ForestMap) : Nat := 
  map.rows - 1

structure TreeWithPosition extends Tree where
  row: Nat
  col: Nat
  deriving Inhabited

instance : Repr TreeWithPosition where
  reprPrec (tree: TreeWithPosition) _ :=
    .text s!"{tree.height}({tree.row}×{tree.col})"

def ForestMap.walkTrees (map: ForestMap) : Array TreeWithPosition := Id.run <| do
  let mut l : Array TreeWithPosition :=  #[]
  for row in [0:map.rows] do
    for column in [0:map.columns] do
      l := l.push <| TreeWithPosition.mk map.trees[row]![column]! row column
  l

#eval sampleMap.walkTrees |>.size
#eval sampleMap.walkTrees[8]?

inductive Direction where
  | up
  | down
  | left
  | right

def ForestMap.isVibileInDirection (map: ForestMap) (direction: Direction) (pos: TreeWithPosition) : Bool := Id.run <| do
  let (rows, cols) :=
    (match direction with
    | .up => ([:pos.row], [pos.col:pos.col.succ])
    | .down => ([pos.row + 1:map.rows], [pos.col:pos.col.succ])
    | .left => ([pos.row:pos.row.succ], [:pos.col])
    | .right => ([pos.row:pos.row.succ], [pos.col + 1:map.columns]))

  for r in rows do
    for c in cols do
      if map.trees[r]![c]!.height >= pos.height then
        return false

  return true

#eval sampleMap.isVibileInDirection Direction.up {height:=5, col:=1, row:=1}

def ForestMap.isEdgeTree (map: ForestMap) (tree: TreeWithPosition) : Bool :=
  tree.row = 0 
  ∨ tree.col = 0 
  ∨ tree.row = map.lastRow
  ∨ tree.col = map.lastColumn

def ForestMap.isVisible (map: ForestMap) (tree: TreeWithPosition) : Bool := Id.run <| do
  if map.isEdgeTree tree then true
  else
  for dir in [Direction.up, Direction.down, Direction.left, Direction.right] do
    if map.isVibileInDirection dir tree then return true
  return false

def visibleTrees (map: ForestMap) : Array TreeWithPosition :=
  map.walkTrees
  |>.filter map.isVisible

#eval visibleTrees sampleMap

#eval visibleTrees sampleMap |>.size
#eval visibleTrees sampleMap |>.filter (λ tree => ¬ sampleMap.isEdgeTree tree)

def countVisibleTrees (map: ForestMap) : Nat :=
  visibleTrees map
  |>.size

def part₁ (input: Input) : Nat := 
  input.lines
  |>.map (·.text)
  |> ForestMap.parse
  |> countVisibleTrees

/--
Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree house: they would like to be able to see a lot of trees.

To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)

The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has large eaves to keep it dry, so they wouldn't be able to see higher than the tree house anyway.

In the example above, consider the middle 5 in the second row:

```
30373
25512
65332
33549
35390
```

Looking up, its view is not blocked; it can see 1 tree (of height 3).
Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
Looking right, its view is not blocked; it can see 2 trees.
Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view).
A tree's scenic score is found by multiplying together its viewing distance in each of the four directions. For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).

However, you can do even better: consider the tree of height 5 in the middle of the fourth row:

```
30373
25512
65332
33549
35390
```

- Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
- Looking left, its view is not blocked; it can see 2 trees.
- Looking down, its view is also not blocked; it can see 1 tree.
- Looking right, its view is blocked at 2 trees (by a massive tree of height 9).

This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.

Consider each tree on your map. What is the highest scenic score possible for any tree?
-/
def toList (range: Std.Range) : List Nat := Id.run <| do
  let mut items := #[]
  for x in range do
    items := items.push x
  return items.toList

def ForestMap.distanceToBlockerInDirection (map: ForestMap) (direction: Direction) (pos: TreeWithPosition) : Nat := Id.run <| do
  let colRange := toList [pos.col:pos.col.succ]
  let rowRange := toList [pos.row:pos.row.succ]
  let (rows, cols) :=
    (match direction with
    | .up => (toList [1:pos.row] |>.reverse, colRange)
    | .down => (toList [pos.row + 1:map.lastRow], colRange)
    | .left => (rowRange, toList [1:pos.col] |>.reverse)
    | .right => (rowRange, toList [pos.col + 1:map.lastColumn]))

  let mut i := 1
  for r in rows do
    for c in cols do
      if map.trees[r]![c]!.height >= pos.height then
        return i
      else i := i+1

  return i

def ForestMap.visibility (map: ForestMap) (tree: TreeWithPosition) : Nat := Id.run <| do
  if map.isEdgeTree tree then 0
  else
  let mut dist := 1
  for dir in [Direction.up, Direction.down, Direction.left, Direction.right] do
    dist := dist * map.distanceToBlockerInDirection dir tree
  return dist

def expectedBest : TreeWithPosition := {height:=5, col:=2, row:=3}
#eval sampleMap.distanceToBlockerInDirection .up expectedBest
#eval sampleMap.distanceToBlockerInDirection .left expectedBest
#eval sampleMap.distanceToBlockerInDirection .down expectedBest
#eval sampleMap.distanceToBlockerInDirection .right expectedBest
#eval sampleMap.visibility expectedBest

def problemTree : TreeWithPosition :=  {height:=6, row:=2, col:=0}
#eval sampleMap.visibility problemTree
#eval sampleMap.distanceToBlockerInDirection .up problemTree
#eval sampleMap.distanceToBlockerInDirection .left problemTree
#eval sampleMap.distanceToBlockerInDirection .down problemTree
#eval sampleMap.distanceToBlockerInDirection .right problemTree

#eval (sampleMap.walkTrees
  |>.map (λ tree => (tree, sampleMap.visibility tree))
  |>.toList)

def bestTreeVisibilityScore (map: ForestMap) : Nat :=
  map.walkTrees
  |>.map (map.visibility ·)
  |>.toList
  |>.maximum?
  |>.get!

#eval bestTreeVisibilityScore sampleMap

def part₂ (input: Input) : Nat := 
  input.lines
  |>.map (·.text)
  |> ForestMap.parse
  |> bestTreeVisibilityScore

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 


#eval testPart₁ solution sample (expect:=21)
#eval testPart₂ solution sample (expect:=8)

