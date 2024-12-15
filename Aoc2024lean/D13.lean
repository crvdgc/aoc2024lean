import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Internal.Parsec

namespace D13

open Std.Internal
open Parsec
open String
open Std

abbrev P := Int × Int

def parseButton (name : String) : Parser P := do
  skipString s!"Button {name}: X+"
  let x ← digits
  skipString s!", Y+"
  let y ← digits
  pure (x, y)

def parsePrize : Parser P := do
  skipString "Prize: X="
  let x ← digits
  skipString ", Y="
  let y ← digits
  pure (x, y)

def solve (a : P) (b : P) (p : P) : Option Int :=
  let bNum := a.fst * p.snd - a.snd * p.fst
  let bDen := a.fst * b.snd - a.snd * b.fst
  if bNum % bDen == 0
    then
      let bCount := bNum / bDen
      let aNum := p.fst - b.fst * bCount
      if aNum % a.fst == 0
        then
          let aCount := aNum / a.fst
          .some (3 * aCount + bCount)
        else .none
    else .none

def part1 (input : Array String) := do
  let mut acc := 0
  for i in [:input.size:4] do
    let a := parse! (parseButton "A") input[i]!
    let b := parse! (parseButton "B") input[i + 1]!
    let p := parse! parsePrize input[i + 2]!
    let res := solve a b p
    if let .some res := res
      then acc := acc + res
  println! s!"{acc}"

def part2 (input : Array String) := do
  let mut acc := 0
  for i in [:input.size:4] do
    let a := parse! (parseButton "A") input[i]!
    let b := parse! (parseButton "B") input[i + 1]!
    let p := parse! parsePrize input[i + 2]!
    let res := solve a b (p.both (. + 10000000000000))
    if let .some res := res
      then acc := acc + res
  println! s!"{acc}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D13
