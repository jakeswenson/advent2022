import Cli
import Advent22

open Cli
open Days

def runDayCmd (p : Parsed) : IO UInt32 := do
  let day: ProblemNumber := p.positionalArg! "day" |>.as! Nat |> ProblemNumber.of

  let problem := List.find? (fun (p: Days.Problem String) => p.day == day) Advent22.days

  IO.println s!"Running day: {day}"
  
  let currentDir ← IO.currentDir
  let dayDir: System.FilePath := currentDir / s!"problems/{day}"
  let inputPath: System.FilePath := dayDir / "problem.txt"

  match problem with 
  | some problem => 
    let problemInput ← IO.FS.readFile inputPath
    let input: Days.Input := Days.Input.mk problemInput
    problem.run input
  | none => IO.println s!"Unable to find problem for day {day}... has it be added to the days list?"
  
  return 0

def makeProblem (p : Parsed) : IO UInt32 := do
  let day: String := p.positionalArg! "day" |>.as! Nat |> ProblemNumber.ofNat! |> Problem.padDay
  let currentDir ← IO.currentDir
  let dayDir: System.FilePath := currentDir / s!"problems/{day}"

  let dirExists ← dayDir.isDir

  if dirExists then 
    IO.println "Problem directory already exists!"
    return 0
  else

  IO.println <| s!"Creating problem structure for day: {day} in dir {dayDir.normalize}"
  IO.FS.createDirAll dayDir

  let inputPath: System.FilePath := dayDir / "problem.txt"

  IO.FS.writeFile inputPath ""

  return 0


def day := `[Cli|
  day VIA runDayCmd; "Runs the problem for a day"

  ARGS:
    day: Nat; "The day to run"
]

def createProblem := `[Cli|
  create VIA makeProblem; "Generates the problem for a day"

  ARGS:
    day: Nat; "The day to create"
]

namespace Advent.CLI

partial def doNothing (p : Parsed) : IO UInt32 := do
  p.printHelp
  return 0

def adventCli : Cmd := `[Cli|
  advent VIA doNothing; ["0.0.0"]
  "Advent 2022 runner"

  SUBCOMMANDS:
    day;
    createProblem

  EXTENSIONS:
    author "jakeswenson"
]