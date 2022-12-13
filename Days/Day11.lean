import Days
import Days.Common
import Std
import Lean.Data.Parsec

open Days
open Days.Common
open Std (RBSet Queue RBMap)
open Lean.Parsec
open Lean (Parsec)

namespace Days.Day11
def day: ProblemNumber := 11

def sample := "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"

structure Item where
  worry: Nat
  deriving Inhabited

instance : Repr Item where
  reprPrec i _ := .text s!"w={i.worry}"

instance : OfNat Item n where
  ofNat := .mk n

abbrev Items := Array Item

abbrev MonkeyId := Nat

structure Monkey where
  id: MonkeyId
  items: Items
  worryModifier: Nat → Nat
  testVal: Nat
  trueTarget: MonkeyId
  falseTarget: MonkeyId
  observedItems: Nat := 0

instance : Inhabited Monkey where
  default := Monkey.mk 0 ∅ id 1 0 0 0

instance : Repr Monkey where
  reprPrec m _ := .text s!"Moneky({m.id}) \{ items := {reprArg m.items} t:{m.trueTarget} f:{m.falseTarget} }" 

abbrev Map (key: Type) [Ord key]  (value: Type) := RBMap key value compare

abbrev MonkeyMap := Map MonkeyId Monkey

structure WorryState where
  worry: Nat

export Functor (map)

/--
Needs to parse strings like:
- `Operation: new = old * 19`
- `Operation: new = old * old`
- `Operation: new = old + 6`
- `Operation: new = old * 19`
-/
def parseWorryOperation : Parsec $ Nat -> Nat := do
  _ ← pstring "Operation: new = "
  return ← operation
  where
    opFunction: Parsec $ Nat → Nat → Nat := do
      let op ← ws *> (pchar '*' <|> pchar '+') <* ws
      return match op with
      | '*' => fun (x₁ x₂: Nat) => x₁ * x₂
      | '+' => fun (x₁ x₂: Nat) => x₁ + x₂
      | u => fun (_ _: Nat) => panic! s!"Unknown op: {u}"
    operand : Parsec $ Nat → Nat :=
      return match ← pstring "old" <|> many1Chars digit with
      | "old" => id
      | digits => 
        let num := String.toNat! digits
        fun _ => num
    operation : Parsec $ Nat → Nat := do
      let op₁ ← operand
      let f ← opFunction
      let op₂ ← operand
      return fun (worry: Nat) => 
        f (op₁ worry) (op₂ worry)

def parseNat : Parsec Nat := map String.toNat! $ many1Chars digit
def parseItemsList: Parsec $ Items := many1 (map Item.mk $ parseNat <* (attempt (pchar ',' *> ws) <|> skip))

#eval "1, 2, 3".iter |> parseItemsList

/--
Parses:

```
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
```
-/
def parseMonkey: Parsec Monkey := do
  let monkeyNumber ← ws *> pstring "Monkey" *> ws *> parseNat <* pchar ':' <* ws
  let items: Array Item ← pstring "Starting items:" *> ws *> parseItemsList <* ws
  let itemQueue : Items := items -- (∅ : Items).enqueueAll items.toList
  let op ← ws *> parseWorryOperation <* ws
  let test ← testVal
  let trueTarget ← pstring "If true: throw to monkey " *> parseNat <* ws
  let falseTarget ← pstring "If false: throw to monkey " *> parseNat <* ws

  return Monkey.mk monkeyNumber itemQueue op test trueTarget falseTarget 0
  where
    testVal := pstring "Test: divisible by " *> ws *> parseNat <* ws

def parseMonkeys : Parsec MonkeyMap := do
  let allMonkeys ← many1 parseMonkey
  return allMonkeys.foldl (init:=∅) (λ map m => 
    map.insert m.id m
  )

#eval parseMonkeys sample.iter

def monkey₀ := Monkey.mk 0 #[79, 98] (·*19) 23 2 3 0
def monkey₁ := Monkey.mk 1 #[54, 65, 75, 74] (·+6) 19 2 0 0
def monkey₂ := Monkey.mk 2 #[79, 60, 97] (λ x => x * x) 13 1 3 0
def monkey₃ := Monkey.mk 3 #[74] (·+3) 17 0 1 0

def monkeyExamples : MonkeyMap := 
  [
    (0, monkey₀),
    (1, monkey₁),
    (2, monkey₂),
    (3, monkey₃)
  ].toRBMap compare

def processMonkeyItem (monkey: Monkey) (worryReducer: Nat → Nat) (map: MonkeyMap) (item: Item) : MonkeyMap := 
  let worryFromInspection := monkey.worryModifier item.worry
  -- After each monkey inspects an item but before it tests your worry level, 
  -- your relief that the monkey's inspection didn't damage the item 
  -- causes your worry level to be divided by three and rounded down to the nearest integer.
  let worryAfterInspection : Nat := worryReducer worryFromInspection

  -- Cheat: Since all monkeys test for division using primes we can just use
  -- modular arithmetic under the ring of (m₁.testVal * m₂.testVal * ...)
  -- (is that the LCM? i think so since they're all primes...)
  -- This works because modular math and at least one of the monkeys tests will always match
  -- First i thought about finding a co-prime to be a divisor, 
  -- but then they're all primes (and the example was 3 which would have been co-prime)... 
  -- so that would be easy and also didn't work
  let coprimeReduction (n: Nat) : Nat :=
    let ring := map.foldl (init:=1) (λ co _ v => co * v.testVal)
    n % ring
  let boundedWorry := coprimeReduction worryAfterInspection
  let target := 
    if divisibleBy monkey.testVal boundedWorry 
    then monkey.trueTarget 
    else monkey.falseTarget
  
  let newItem : Item := Item.mk boundedWorry

  map.modify target (λ m => 
      let items: Items := #[].append m.items |>.push newItem
      { m with items }
  )
  where
    divisibleBy (n val: Nat) : Bool := val % n == 0

def processMonkey (monkey: Monkey) (map: MonkeyMap) (worryReducer: Nat → Nat := (·/3)) : MonkeyMap :=
  monkey.items.foldl (init:=removedItems) (processMonkeyItem monkey worryReducer)
  where 
    removedItems := map.modify monkey.id (λ m => { m with items := ∅, observedItems := monkey.observedItems + monkey.items.size })

#eval monkeyExamples
  |> processMonkey monkey₀ 
#eval monkeyExamples
  |> processMonkey monkey₀  
  |> processMonkey monkey₁
#eval monkeyExamples 
  |> processMonkey monkey₀
  |> processMonkey monkey₁
  |> processMonkey monkey₂

#eval monkeyExamples
  |> (λ x => processMonkey (x.find? 0).get! x)
  |> (λ x => processMonkey (x.find? 1).get! x)
  |> (λ x => processMonkey (x.find? 2).get! x)
  |> (λ x => processMonkey (x.find? 3).get! x)

#eval monkeyExamples
  |> processMonkey monkey₀  
  |> processMonkey monkey₁
  |> processMonkey monkey₂
  |> processMonkey monkey₃
  |>.find? 1

#eval monkeyExamples
  |> processMonkey monkey₀  
  |> processMonkey monkey₁
  |> processMonkey monkey₂
  |>.find? 3

def monkeysRound (map: MonkeyMap) (worryReducer: Nat → Nat := (·/3)) : MonkeyMap :=
  map.foldl (init:=map) (λ map id _ => 
    processMonkey (map.find? id).get! map worryReducer
  )

#eval monkeyExamples |> monkeysRound

def parseAll [ToInput i] (input: i) : MonkeyMap :=
  match parseMonkeys (toInput input).iter with
  | .success _ monkies => monkies
  | .error pos msg => panic! s!"Oops: {msg} @ {pos}"

def runRounds (rounds: Nat) (worryReducer: Nat → Nat) : StateM (MonkeyMap × Array (Nat ×  MonkeyMap)) Unit := do
  for _round in [1:rounds+1] do
    let (current, history) ← get
    let round := monkeysRound current worryReducer
    set $ (round, history.push (_round, round))

abbrev MonkeyCountMap := Map MonkeyId Nat

def roundHistory [ToInput κ] (input: κ) (rounds: Nat) (worryReducer: Nat → Nat := (·/3)) : Array $ Nat × MonkeyMap :=
  let initialMonkeys := parseAll (toInput input)
  let result := StateT.run (runRounds rounds worryReducer) (initialMonkeys, #[(0, initialMonkeys)]) 
  let (_, history): MonkeyMap × Array (Nat × MonkeyMap) := result.snd
  history

#eval roundHistory sample 20

def run [ToInput κ] (input: κ) (rounds: Nat) (worryReducer: Nat → Nat := (·/3)) : Array Nat :=
  roundHistory input rounds worryReducer
  |>.map (λ ((_, round): Nat × MonkeyMap) => 
    round.foldl (init:=∅) (λ (state: MonkeyCountMap) (k: MonkeyId) (m: Monkey) => 
      state.insert k m.observedItems
    )
  )
  |>.lastD ∅
  |>.valuesArray
  |>.qsort (Ordering.lt == compare · ·)

#eval run sample 20
/--
-/
def part₁ (input: Input) : Nat :=
  let rounds := 20
  run input rounds
  |>.lastN 2
  |> Subarray.toArray
  |>.toList
  |> mul 1

/--
-/
def part₂ (input: Input) : Nat :=
  let rounds := 10000
  run input rounds id
  |>.lastN 2
  |> Subarray.toArray
  |>.toList
  |> mul 1

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 

#eval testPart₁ solution sample (expect:=10605)
-- #eval testPart₂ solution sample (expect:=0)

