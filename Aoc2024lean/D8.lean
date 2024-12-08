import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet

namespace D8

open Std
open HashMap
open HashSet

abbrev P := Int × Int

def parse (input : Array String) : P × List (List P)  := Id.run do
  let mut m : HashMap Char (List P) := HashMap.empty
  for (line, x) in input.zipWithIndex do
    for c in line.toList, y in [:line.length] do
      if c == '.' then
        continue
      else
        m := m.alter c <| fun ps =>
          let ps := ps.getD []
          .some <| (x, y) :: ps
  return ((input.size, input[0]!.length), m.values)

def antinodes (p₁ : P) (p₂ : P) : P × P :=
  let sym axis x := 2 * axis - x
  ((sym p₁.fst p₂.fst, sym p₁.snd p₂.snd), (sym p₂.fst p₁.fst, sym p₂.snd p₁.snd))

-- #eval antinodes (3, 4) (5, 5)

def pairs : List α → List (α × α)
  | [] => []
  | x :: xs => xs.map (Prod.mk x) ++ pairs xs

-- #eval pairs [1,2,3]

def inBounds (bounds : P) (p : P) : Bool :=
  0 ≤ p.fst && p.fst < bounds.fst &&
  0 ≤ p.snd && p.snd < bounds.snd

def inBoundAntinodes (bounds : P) (p₁ : P) (p₂ : P) : List P :=
  let (a₁, a₂) := antinodes p₁ p₂
  [a₁, a₂].filter (inBounds bounds)

def part1 (input : Array String) :=
  let (bounds, signals) := parse input
  let inBoundAntinodes := Function.uncurry (inBoundAntinodes bounds)
  let count :=
    signals
      |>.flatMap (fun ps => pairs ps |>.flatMap inBoundAntinodes)
      |> HashSet.ofList
      |>.size
  println! s!"{count}"

def part2 (input : Array String) :=
  println! s!"{input}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D8
