import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet

namespace D10

open Std

def parse (input : Array String) : HashSet (Int × Int) × HashMap (Int × Int) Nat :=
  input.zipWithIndex.foldl
    (fun (starts, map) (line, r) =>
      line.toList.toArray.zipWithIndex.foldl
        (fun (starts, map) (n, c) =>
          let n := n.toNat - '0'.toNat
          let starts := if n == 0 then starts.insert (r, c) else starts
          let map := map.insert (r, c) n
          (starts, map)
        )
        (starts, map)
    )
    (HashSet.empty, HashMap.empty)

def neighboursInBound (bound : Int × Int) (p : Int × Int) : List (Int × Int) :=
  let (x', y') := bound
  let inBound p :=
    let (x, y) := p
    0 ≤ x && x < x' && 0 ≤ y && y < y'
  let (x, y) := p
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)].filter inBound

def countEnds (bound : Int × Int) (start : Int × Int) (map : HashMap (Int × Int) Nat) : Nat :=
  Id.run do
    let mut visited : HashSet (Int × Int) := HashSet.empty
    let mut count := 0
    let mut q := Array.singleton start
    while !q.isEmpty do
      let (cur, q') := q.popFront!
      q := q'
      let curN := map[cur]!
      if curN == 9 then
        count := count + 1
      else
        for neighbor in neighboursInBound bound cur do
          let neighborN := map[neighbor]!
          if neighborN = curN + 1 && !visited.contains neighbor then
            visited := visited.insert neighbor
            q := q.push neighbor
    return count

def part1 (input : Array String) := do
  let (starts, map) := parse input
  let bound : Int × Int := (input.size, input[0]!.length)
  let count := starts.fold
    (fun acc start => acc + countEnds bound start map)
    0
  println! s!"{count}"

def part2 (input : Array String) :=
  println! s!"{input}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D10
