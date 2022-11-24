-- lake docs: https://github.com/leanprover/lake
import Lake
open Lake DSL


package adevent22 {
  -- add package configuration options here
}

-- Dependencies: https://github.com/leanprover/lake#syntax-of-require

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

require std from git "https://github.com/leanprover/std4" @ "main"

-- CLI apps: https://github.com/mhuisi/lean4-cli (needs to use nightly to build...)
require Cli from git
  "https://github.com/mhuisi/lean4-cli.git" @ "nightly"

lean_lib Days {
  -- add library configuration options here
}

lean_lib Advent22 {
  -- add library configuration options here
}

lean_lib AdventCLI {
  -- add library configuration options here
}

@[default_target]
lean_exe advent {
  root := `Main
}
