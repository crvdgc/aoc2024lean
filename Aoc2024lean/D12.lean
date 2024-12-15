import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet

namespace D12

open Std

abbrev P := Int × Int

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
        let mut q : Array P := #[(r, c)]
        while !q.isEmpty do
          let ((r, c), q') := q.popFront!
          q := q'
          visited := visited.insert (r, c)
          area := area + 1
          let neighbors :=
            neighboursInBound (m, n) (r, c)
              |>.filter (fun (p : P) => map[p.fst.toNat]![p.snd.toNat]! == curC)
          perimeter := perimeter + 4 - neighbors.length
          for neighbor in neighbors do
            if !visited.contains neighbor && !q.contains neighbor then
              q := q.push neighbor
        acc := acc + area * perimeter
  println! s!"{acc}"

def part2 (input : Array String) := do
  let map := parse input
  let (m, n) := (input.size, map[0]!.size)
  let inBound (p : P) :=
    let (x, y) := p
    0 ≤ x && x < m && 0 ≤ y && y < n
  let mut acc := 0
  let mut visited : HashSet P := HashSet.empty
  for r in [:m] do
    for c in [:n] do
      if visited.contains (r, c) then
        continue
      else
        let curC := map[r]![c]!
        let mut area := 0
        let mut vertical : HashSet (Bool × P) := HashSet.empty -- isLeftOrTop
        let mut horizontal : HashSet (Bool × P) := HashSet.empty
        let mut q : Array P := #[(r, c)]
        while !q.isEmpty do
          let ((r, c), q') := q.popFront!
          q := q'
          visited := visited.insert (r, c)
          area := area + 1
          for p in neighboursInBound (m, n) (r, c) do
            if map[p.fst.toNat]![p.snd.toNat]! == curC && !q.contains p && !visited.contains p
              then q := q.push p
          let shouldAddEdge p := !inBound p || map[p.fst.toNat]![p.snd.toNat]! != curC
          -- top
          if shouldAddEdge (r - 1, c)
            then horizontal := horizontal.insert (true, r, c)
          -- bottom
          if shouldAddEdge (r + 1, c)
            then horizontal := horizontal.insert (false, r + 1, c)
          -- left
          if shouldAddEdge (r, c - 1)
            then vertical := vertical.insert (true, r, c)
          -- right
          if shouldAddEdge (r, c + 1)
            then vertical := vertical.insert (false, r, c + 1)
        let vertical' := vertical.toArray.qsort <| fun (isTop₁, p₁) (isTop₂, p₂) =>
          if isTop₁ != isTop₂
            then isTop₁
            else
              match compare p₁.snd p₂.snd with
              | .eq => p₁.fst < p₂.fst
              | ord => ord == Ordering.lt
        let horizontal' := horizontal.toArray.qsort <| fun (isTop₁, p₁) (isTop₂, p₂) =>
          if isTop₁ != isTop₂
            then isTop₁
            else
              match compare p₁.fst p₂.fst with
              | .eq => p₁.snd < p₂.snd
              | ord => ord == Ordering.lt
        let count (toPrev : P → P) (ps : Array (Bool × P)) :=
          match ps.popFront? with
          | .none => 0
          | .some (p, ps) => Id.run do
            let mut acc := 1
            let mut prev := p
            let toPrev' (p : Bool × P) := (p.fst, toPrev p.snd)
            for p in ps do
              if toPrev' p != prev
                then acc := acc + 1
              prev := p
            return acc
        let sides := count (fun p => (p.fst - 1, p.snd)) vertical' + count (fun p => (p.fst, p.snd - 1)) horizontal'
        acc := acc + area * sides
  println! s!"{acc}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D12
