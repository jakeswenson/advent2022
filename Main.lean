import Advent22
import AdventCLI

open Advent.CLI

def main (args : List String) : IO UInt32 :=
  adventCli.validate args

#eval main <| "day 01".splitOn " "
#eval main <| "--version".splitOn " "
 