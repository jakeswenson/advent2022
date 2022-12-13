namespace List

def sum [Add α] [Inhabited α]: List α → α := List.foldl (init:=default) (·+·)
def mul [Mul α] (default: α) : List α → α := List.foldl (init:=default) (·*·)

def windowed {α : Type} (size: Nat) (items: List α) : List $ List α :=
  items
  |>.enum
  |>.groupBy (λ 
  | (idx₁, _), (idx₂, _) => idx₁ / size == idx₂ / size
  )
  |>.map (List.map (·.snd))

end List

namespace Array

def last {α: Type} (array: Array α) (arrayHasItems: array.size - 1 < array.size := by decide): α :=
  let index: Fin array.size := Fin.mk (array.size - 1) arrayHasItems
  array.get index

def lastD {α: Type} (array: Array α) (default: α): α :=
  array.getD (array.size - 1) default

def lastN {α: Type} (array: Array α) (n: Nat): Subarray α :=
  array[array.size - n:]

end Array

namespace Std

def Std.Range.foldl {α: Type} (f: α → Nat → α) (init: α) (range: Std.Range) : α := 
  let (_, state) := StateT.run m init
  state
  where 
    m: StateM α Unit := do
      for r in range do
        set $ f (← get) r

end Std

namespace Days.Common

export List (sum mul windowed)
export Array (last lastD lastN)

def Function.curry {α β χ : Type} (f: α × β → χ) : (α → β → χ) :=
  λ (x: α) => λ (y: β) => f ⟨ x, y ⟩

def Function.uncurry {α β χ : Type} (f: α → β → χ) : (α × β → χ) :=
  λ (pair: α × β) => f pair.fst pair.snd

def Option.orDefault (o: Option α) (defaultValue: α) : α :=
  match o with
  | some v => v
  | none => defaultValue

export Option (orDefault)