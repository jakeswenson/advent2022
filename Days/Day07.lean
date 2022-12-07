import Days
import Days.Common
import Std
import Lean.Data.Parsec

open Days
open Days.Common
namespace Days.Day07
def day: ProblemNumber := 07

open Std (RBMap mkRBMap)

open Lean.Parsec
open Lean (Parsec)

/--
You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear much louder sounds in the distance; how big do the animals get out here, anyway?

The device the Elves gave you has problems with more than just its communication system. You try to run a system update:

```
$ system-update --please --pretty-please-with-sugar-on-top
Error: No space left on device
```

Perhaps you can delete some files to make space for the update?

You browse around the filesystem to assess the situation and save the resulting terminal output (your puzzle input). For example:

```
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
```

The filesystem consists of a tree of files (plain data) and directories (which can contain other directories or files). The outermost directory is called `/`. You can navigate around the filesystem, moving into or out of directories and listing the contents of the directory you're currently in.

Within the terminal output, lines that begin with `$` are **commands you executed**, very much like some modern computers:

- `cd` means **change directory**. This changes which directory is the current directory, but the specific result depends on the argument:
  - `cd x` moves **in** one level: it looks in the current directory for the directory named `x` and makes it the current directory.
  - `cd ..` moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
  - `cd /` switches the current directory to the outermost directory, `/`.
- `ls` means list. It prints out all of the files and directories immediately contained by the current directory:
  - `123 abc` means that the current directory contains a file named `abc` with size `123`.
  - `dir xyz` means that the current directory contains a directory named `xyz`.

Given the commands and output in the example above, you can determine that the filesystem looks visually like this:

```
- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)
```

Here, there are four directories: `/` (the outermost directory), `a` and `d` (which are in `/`), and `e` (which is in `a`). These directories also contain files of various sizes.

Since the disk is full, your first step should probably be to find directories that are good candidates for deletion. To do this, you need to determine the **total size** of each directory. The total size of a directory is the sum of the sizes of the files it contains, directly or indirectly. (Directories themselves do not count as having any intrinsic size.)

The total sizes of the directories above can be found as follows:

- The total size of directory `e` is **584** because it contains a single file `i` of size 584 and no other directories.
- The directory `a` has total size **94853** because it contains files `f` (size 29116), `g` (size 2557), and `h.lst` (size 62596), plus file `i` indirectly (`a` contains `e` which contains `i`).
- Directory `d` has total size **24933642**.
- As the outermost directory, `/` contains every file. Its total size is **48381165**, the sum of the size of every file.

To begin, find all of the directories with a total size of **at most 100000**, then calculate the sum of their total sizes. In the example above, these directories are `a` and `e`; the sum of their total sizes is **95437** (94853 + 584). (As in this example, this process can count files more than once!)

Find all of the directories with a total size of at most 100000. 

**What is the sum of the total sizes of those directories?**
-/

abbrev DirName := String

inductive ChangeDir where
  | up : ChangeDir
  | root : ChangeDir
  | dir (name: DirName) : ChangeDir
  deriving Repr, BEq, DecidableEq

abbrev FileSize := Nat
abbrev FileName := String

inductive FsEntry where
  | directory (parent: List DirName) (name: DirName) : FsEntry
  | file (size: FileSize) (name: FileName) : FsEntry
  deriving Repr, BEq, DecidableEq

inductive Command where
  | cd (parent: List DirName) (target: ChangeDir) : Command
  | ls (dir: List DirName) (children: List FsEntry) : Command
  deriving Repr, BEq, DecidableEq

def mkAbsolutePath (dir: List DirName) := (String.intercalate "/" dir.reverse)

def wordUntilSpaceOrNewLine : Parsec String :=
      many1Chars (do 
        let c ← peek?
        if c != some ' ' ∧ c != some '\n' 
        then anyChar
        else fail "Found space")

def parseCd (parent: List DirName) : Parsec Command := do
  _ ← pstring "cd" <* pchar ' ' <* ws
  return Command.cd parent <| match ← (pstring "/" <|> pstring ".." <|> wordUntilSpaceOrNewLine) with 
  | "/" => ChangeDir.root
  | ".." => ChangeDir.up
  | dir => ChangeDir.dir dir

#eval ["cd /".iter, "cd ..".iter, "cd foo".iter] |>.map $ parseCd []

def parseLs (parent: List DirName) : Parsec Command := do
  _ ← pstring "ls" <* ws
  return Command.ls parent <| ← parseChildren
  where
    parseDir := do 
      let dirName ← pstring "dir" *>  ws *> wordUntilSpaceOrNewLine <* ws
      return FsEntry.directory parent dirName
    
    parseFile := do
      let size: Nat := (← ws *> many1Chars digit <* ws) |> String.toNat!
      let fileName ← wordUntilSpaceOrNewLine <* ws
      return FsEntry.file size fileName

    parseChildren := do
      return Array.toList <| ← ws *> many (parseDir <|> parseFile)

#eval parseLs [] "ls
dir e
29116 f
2557 g
62596 h.lst".iter

def parseCommand (currentDir: List DirName) : Parsec Command := 
  ws *> (parseCd currentDir <|> parseLs currentDir) <* ws

#eval ["cd /".iter, "ls\ndir name\n1235 test".iter] |>.map $ parseCommand ["foo", "/"]

structure ParseContext where
  currentDir: List DirName
  commands: Array Command

def parseCommands (input: String) : Except String $ List Command :=
  commands.map (Array.toList $ ·.commands)
  where
    commandLines := input.splitOn "$ " |>.filter (¬·.isEmpty)
    commands :=
      commandLines
      |>.map (·.iter)
      |>.foldl (init:=Except.ok (ParseContext.mk [] #[])) λ
      | Except.error e, _ => .error e
      | .ok ctx, line => match parseCommand ctx.currentDir line with
      | .success _ cmd => Except.ok (match cmd with 
        | Command.ls _ _ => { ctx with commands := ctx.commands.push cmd }
        | Command.cd _ dir => {
            ctx with 
              commands := ctx.commands.push cmd, 
              currentDir := match dir with 
                | ChangeDir.root => ["/"]
                | ChangeDir.up => ctx.currentDir.tail!
                | ChangeDir.dir name => name::ctx.currentDir
            } 
        )
      | .error _ msg => Except.error s!"Failed to parse commands: {msg}"

inductive FileSystemTree
  | dir: DirName -> List FileSystemTree -> FileSystemTree
  | file (size: FileSize) (name: FileName) : FileSystemTree

abbrev FsMap := RBMap String (List FsEntry) compare

def buildDirMap (input: String) : Except String $ FsMap := 
  buildTree
  where
    buildTree :=
      parseCommands input
      |>.map (λ res =>
        res
        |>.foldl (init:=mkRBMap String (List FsEntry) compare) λ
          | map, Command.cd _ _ => map
          | map, Command.ls dir children => 
            map.insert (mkAbsolutePath dir) children
      )

partial def dirSize (map: FsMap) (dir: String): Nat :=
  let entries := map.find? dir 
  (match entries with 
  | some a => a 
  | none => [])
  |>.foldl (init:=0) (λ
  | acc, FsEntry.directory parent dir => 
    acc + dirSize map (mkAbsolutePath (dir::parent))
  | acc, FsEntry.file size _ => 
    acc + size
  )

def parseFsMap (input: String) : FsMap :=
  match buildDirMap input with
  | .ok res => res
  | .error msg => panic! s!"Failed to parse input! {msg}"

def findDirSizesMatchingPredicate (filter: Nat → Bool) (map: FsMap) : List Nat :=
  map.keysList
  |>.map (dirSize map ·)
  |>.filter filter

def sumAllLessThanSize (filter: Nat → Bool) (map: FsMap): Nat :=
  findDirSizesMatchingPredicate filter map 
  |> sum

def part₁ (input: Input) : Nat :=
  parseFsMap input.text
  |> sumAllLessThanSize (· ≤ 100000)

/--
Now, you're ready to choose a directory to delete.

The total disk space available to the filesystem is **`70000000`**. To run the update, you need unused space of at least **`30000000`**. You need to find a directory you can delete that will **free up enough space** to run the update.

In the example above, the total size of the outermost directory (and thus the total amount of used space) is `48381165`; this means that the size of the **unused** space must currently be `21618835`, which isn't quite the `30000000` required by the update. Therefore, the update still requires a directory with total size of at least `8381165` to be deleted before it can run.

To achieve this, you have the following options:

Delete directory `e`, which would increase unused space by `584`.
Delete directory `a`, which would increase unused space by `94853`.
Delete directory `d`, which would increase unused space by `24933642`.
Delete directory `/`, which would increase unused space by `48381165`.

Directories `e` and `a` are both too small; deleting them would not free up enough space. However, directories `d` and `/` are both big enough! Between these, choose the smallest: `d`, increasing unused space by **`24933642`**.

Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update. 
**What is the total size of that directory?**
-/
def HDSize := 70000000
def UpdateSizeRequired := 30000000

def findSmallestDirMatchingPredicate (filter: Nat → Bool) (map: FsMap): Nat :=
  findDirSizesMatchingPredicate filter map
  |>.minimum?
  |>.get!

def part₂ (input: Input) : Nat :=
  let fsMap := parseFsMap input.text
  let totalSizeTaken := dirSize fsMap "/"
  let availableSpace := HDSize - totalSizeTaken
  let neededSpace := UpdateSizeRequired - availableSpace
  findSmallestDirMatchingPredicate (. ≥ neededSpace) fsMap
  

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 

def sample := "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

#eval parseCommands sample

def sampleMap := parseFsMap sample

#eval sampleMap

#eval 584 = dirSize sampleMap "//a/e"
#eval 94853 = dirSize sampleMap "//a"
#eval 24933642 = dirSize sampleMap "//d"

#eval testPart₁ solution sample (expect:=95437)
#eval testPart₂ solution sample (expect:=24933642)

