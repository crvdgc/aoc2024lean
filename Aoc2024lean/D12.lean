import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet

namespace D12

open Std

abbrev P := Nat × Nat

def parse (s : Array String) : Array (Array Char) :=
  s.map (List.toArray ∘ String.toList)

def neighboursInBound (bound : P) (p : P) : List P :=
  let (m, n) := bound
  let (x, y) := p
  [
    if x > 0 then .some (x - 1, y) else .none,
    if x + 1 < m then .some (x + 1, y) else .none,
    if y > 0 then .some (x, y - 1) else .none,
    if y + 1 < n then .some (x, y + 1) else .none,
  ].filterMap id

def part1 (input : Array String) := do
  let map := parse input
  let (m, n) := (input.size, map[0]!.size)
  let mut acc := 0
  let mut visited : HashSet P := HashSet.empty
  for r in [:m] do
    for c in [:n] do
      if visited.contains (r, c) then
        continue
      else
        let curC := map[r]![c]!
        let mut area := 0
        let mut perimeter := 0
        let mut q := #[(r, c)]
        while !q.isEmpty do
          let ((r, c), q') := q.popFront!
          q := q'
          visited := visited.insert (r, c)
          area := area + 1
          let neighbors :=
            neighboursInBound (m, n) (r, c)
              |>.filter (fun (p : P) => map[p.fst]![p.snd]! == curC)
          perimeter := perimeter + 4 - neighbors.length
          for neighbor in neighbors do
            if !visited.contains neighbor && !q.contains neighbor then
              q := q.push neighbor
        acc := acc + area * perimeter
  println! s!"{acc}"

def part2 (input : Array String) := do
  println! s!"{input}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D12
