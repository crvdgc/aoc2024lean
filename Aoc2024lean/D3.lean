import Std.Internal.Parsec
import Aoc2024lean.Basic
import Batteries.Data.Vector

namespace D3

open Std.Internal
open Parsec
open String

def oneToThreeDigits : Parser Nat := do
  let n ← digits
  if n > 1000
    then fail "too many digits"
    else pure n

def mul : Parser Nat := do
  skipString "mul("
  let n₁ ← oneToThreeDigits
  skipChar ','
  let n₂ ← oneToThreeDigits
  skipChar ')'
  pure (n₁ * n₂)

partial def muls : Parser Nat := do
  let rec go (acc : Nat) : Parser Nat :=
    tryCatch (attempt mul)
      (fun n => go (acc + n))
      (fun () => (Parsec.String.take 1 *> go acc) <|> pure acc)
  go 0

def part1 (input : Array String) : IO Unit := do
  let ans := input.map (parse! muls) |>.foldl (. + .) 0
  println! s!"{ans}"

def part2 (input : Array String) : IO Unit := do
  println! "part2"

def run (part : String) (input : Array String) : IO Unit := do
  match part with
  | "1" | "1e" | "1e2" => part1 input
  | "2" | "2e" => part2 input
  | _ => println! s!"{part} not implemented"

end D3
