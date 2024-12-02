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
  let ls := Array.sort ls
  let rs := Array.sort rs
  let res :=
    Array.zipWith ls rs (fun x₁ x₂ => if x₁ < x₂ then x₂ - x₁ else x₁ - x₂)
    |> Array.foldl (. + .) 0
  println! s!"{res}"

def run (part : String) (input : Array String) : IO Unit := do
  let n := input.size
  let mut (ls, rs) := (Array.mkArray n 0, Array.mkArray n 0)
  for i in [0:n] do
    let (l, r) := parse! parser input[i]!
    ls := Array.set! ls i l 
    rs := Array.set! rs i r
  match part with
  | "1" | "1e" => part1 ls rs
  | _ => println! s!"hello {ls}, {rs}"

end D1

