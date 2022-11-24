import Cli

open Cli

def runDayCmd (p : Parsed) : IO UInt32 := do
  let day   : Nat       := p.positionalArg! "day" |>.as! Nat
  IO.println <| s!"Running day: {day}"
  return 0

def padDay (day: Nat) : String := 
  if day < 10 then s!"0{day}" else s!"{day}"


def makeProblem (p : Parsed) : IO UInt32 := do
  let day: String := p.positionalArg! "day" |>.as! Nat |> padDay
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

def problem := `[Cli|
  problem VIA makeProblem; "Generates the problem for a day"

  ARGS:
    day: Nat; "The day to run"
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
    problem

  EXTENSIONS:
    author "jakeswenson"
]