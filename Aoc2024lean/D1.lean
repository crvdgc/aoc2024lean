import Std.Internal.Parsec
import Aoc2024lean.Basic

namespace D1

open Std.Internal
open Parsec

def parser : String.Parser (Nat × Nat) := do
  _ ← many (String.skipChar ' ')
  let l ← String.digits
  _ ← many (String.skipChar ' ')
  let r ← String.digits
  pure (l, r)


def part1 (ls : Array Nat) (rs : Array Nat) : IO Unit := do
  let (ls, rs) := (ls.qsortOrd, rs.qsortOrd)
  let res :=
    Array.zipWith ls rs (fun x₁ x₂ => if x₁ < x₂ then x₂ - x₁ else x₁ - x₂)
    |> Array.foldl (. + .) 0
  println! s!"{res}"

def part2 (ls : Array Nat) (rs : Array Nat) : IO Unit := do
  let calculateScore (cur : Nat) (rs : Array Nat) :=
    let (_, rs) := rs.splitWhile (. < cur)
    let (before, after) := rs.splitWhile (. = cur)
    (cur * before.size, after)
  let mut (ls, rs) := (ls.qsortOrd, rs.qsortOrd)
  let mut acc := 0
  let mut last : Option (Nat × Nat) := .none
  while !Array.isEmpty ls do
    let cur := ls[0]!
    let score ← do
      if let .some (lastValue, lastScore) := last then
        if cur = lastValue then
          pure lastScore
        else
          let (score, newRs) := calculateScore cur rs
          rs := newRs
          last := .some (cur, score)
          pure score
      else
        let (score, newRs) := calculateScore cur rs
        rs := newRs
        last := .some (cur, score)
        pure score
    acc := acc + score
    ls := ls.toSubarray (start := 1)
  println! s!"{acc}"

def run (part : String) (input : Array String) : IO Unit := do
  let n := input.size
  let mut (ls, rs) := (Array.mkArray n 0, Array.mkArray n 0)
  for i in [0:n] do
    let (l, r) := parse! parser input[i]!
    ls := Array.set! ls i l 
    rs := Array.set! rs i r
  match part with
  | "1" | "1e" => part1 ls rs
  | "2" | "2e" => part2 ls rs
  | _ => println! s!"hello {ls}, {rs}"

end D1

