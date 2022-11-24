namespace Days

structure Input := 
  text: String 

structure Problem :=
  define ::
  day: Nat
  part1: Input -> Int

def Problem.run: Problem -> Input → Int := 
  fun p i => p.part1 i

def Input.lines : (@& Input) → List String
  | ⟨s⟩ => String.splitOn s "\n"


def Problem.test (p: Problem) (input: String) : Int := 
  p.part1 (Input.mk input)