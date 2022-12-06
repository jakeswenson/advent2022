import Days
import Days.Common
import Std
import Lean.Data.Parsec

open Days
open Days.Common
namespace Days.Day06
def day: ProblemNumber := 06

open Std (RBSet)

/--
he preparations are finally complete; you and the Elves leave camp on foot and begin to make your way toward the star fruit grove.

As you move through the dense undergrowth, one of the Elves gives you a handheld device. He says that it has many fancy features, but the most important one to set up right now is the communication system.

However, because he's heard you have significant experience dealing with signal-based systems, he convinced the other Elves that it would be okay to give you their one malfunctioning device - surely you'll have no problem fixing it.

As if inspired by comedic timing, the device emits a few colorful sparks.

To be able to communicate with the Elves, the device needs to lock on to their signal. The signal is a series of seemingly-random characters that the device receives one at a time.

To fix the communication system, you need to add a subroutine to the device that detects a start-of-packet marker in the datastream. In the protocol being used by the Elves, **the start of a packet is indicated by a sequence of four characters that are all different**.

The device will send your subroutine a datastream buffer (your puzzle input); your subroutine needs to identify the first position where the four most recently received characters were all different. Specifically, it needs to report the number of characters from the beginning of the buffer to the end of the first such four-character marker.

For example, suppose you receive the following datastream buffer:

`mjqjpqmgbljsphdztnvjfqwrcgsmlb`
After the first three characters (mjq) have been received, there haven't been enough characters received yet to find the marker. The first time a marker could occur is after the fourth character is received, making the most recent four characters mjqj. Because j is repeated, this isn't a marker.

The first time a marker appears is after the seventh character arrives. Once it does, the last four characters received are jpqm, which are all different. In this case, your subroutine should report the value 7, because the first start-of-packet marker is complete after 7 characters have been processed.

Here are a few more examples:

- `bvwbjplbgvbhsrlpgdmjqwftvncz`: first marker after character 5
- `nppdvjthqldpwncqszvftbrmjlhg`: first marker after character 6
- `nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg`: first marker after character 10
- `zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw`: first marker after character 11

How many characters need to be processed before the first start-of-packet marker is detected?
-/
def part₁ (input: Input) : Nat := 
  firstMarker input.text
  where
    scanFirst : Nat → List Char → Option Nat
    | idx, m₁::m₂::m₃::m₄::rest => 
      if 4 = (Std.mkRBSet Char compare
      |>.insert m₁
      |>.insert m₂
      |>.insert m₃
      |>.insert m₄
      |>.size)
      then some <| idx + 4
      else scanFirst (idx+1) (m₂::m₃::m₄::rest)
    | _, _ => none 

    firstMarker (line: String) : Nat := 
      match scanFirst 0 line.data with
      | some n => n
      | _ => 0

/--
Your device's communication system is correctly detecting packets, but still isn't working. It looks like it also needs to look for **messages**.

A **start-of-message marker** is just like a start-of-packet marker, except it consists of **14 distinct characters** rather than 4.

Here are the first positions of start-of-message markers for all of the above examples:

- `mjqjpqmgbljsphdztnvjfqwrcgsmlb`: first marker after character 19
- `bvwbjplbgvbhsrlpgdmjqwftvncz`: first marker after character 23
- `nppdvjthqldpwncqszvftbrmjlhg`: first marker after character 23
- `nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg`: first marker after character 29
- `zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw`: first marker after character 26

How many characters need to be processed before the first start-of-message marker is detected?
-/

structure CircularBuffer (α : Type) (size: Nat) where
  buffer: Array α
  position: Nat

def CircularBuffer.push {size: Nat} {α: Type} (buffer: CircularBuffer α size) (x: α) : CircularBuffer α size :=
  let newBuffer := buffer.buffer.setD buffer.position x
  { buffer := newBuffer, position := (buffer.position + 1) % size }

def MessageSize := 14
def startOfMessage (s: String) : Nat :=
  if h: s.length ≥ MessageSize then firstMessageStart h else 0
  where 
    scan {bufSize: Nat} (idx: Nat) (buffer: CircularBuffer Char bufSize) (chars: List Char) : Option Nat :=
      match chars with
      | c::rest =>
        let set := Std.RBSet.ofArray buffer.buffer compare
        if set.size = bufSize 
        then some (idx + bufSize)
        else scan (idx+1) (buffer.push c) rest
      | [] => none
    firstMessageStart (_: s.length ≥ MessageSize) :=
      let array: Array Char := s.data.take MessageSize |> List.toArray
      let buf: CircularBuffer Char MessageSize := CircularBuffer.mk array 0
      let scanAfter := s.data.drop MessageSize
      match scan 0 buf scanAfter with 
      | some idx => idx
      | none => 0 

def part₂ (input: Input) : Nat :=
  startOfMessage input.text

def solution : Problem Nat := ⟨ day, part₁, part₂ ⟩ 

def sample := "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

def sampleInput := Input.mk sample

#eval testPart₁ solution sample (expect:=7)
#eval testPart₂ solution sample (expect:=19)

