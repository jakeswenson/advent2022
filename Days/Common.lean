namespace Days.Common

def sum [Add α] [Inhabited α] (items: List α) : α :=
  List.foldl (init:=default) (·+·) items

def List.windowed {α : Type} (size: Nat) (items: List α) : List $ List α :=
  items
  |>.enum
  |>.groupBy (λ 
  | (idx₁, _), (idx₂, _) => idx₁ / size == idx₂ / size
  )
  |>.map (List.map (·.snd))

export List (windowed)

def Function.curry {α β χ : Type} (f: α × β → χ) : (α → β → χ) :=
  λ (x: α) => λ (y: β) => f ⟨ x, y ⟩

def Function.uncurry {α β χ : Type} (f: α → β → χ) : (α × β → χ) :=
  λ (pair: α × β) => f pair.fst pair.snd 