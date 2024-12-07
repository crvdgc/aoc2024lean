import Aoc2024lean.Basic

namespace D5

def parse (input : Array String) : Array (Nat × Nat) × Array (Array Nat) :=
  let (rules, prints) := input.splitWhile (. != "")
  let rules := rules.map <| fun s =>
    let cs := s.splitOn "|"
    (cs[0]!.toNat!, cs[1]!.toNat!)
  let prints := prints.tail.map <| fun s =>
    s.splitOn "," |>.map String.toNat! |>.toArray
  (rules, prints)

def isValid (rules : Array (Nat × Nat)) (xs : Array Nat) : Bool :=
  let sorted := xs.qsort (fun a b => rules.contains (a, b))
  xs == sorted

def findMid (xs : Array Nat) : Nat :=
  let mid := xs.size.div 2
  xs[mid]!

def part1 (input : Array String) :=
  let (rules, prints) := parse input
  let res :=
    prints
      |>.filter (isValid rules)
      |>.map findMid
      |>.sum
  println! s!"{res}"

def reorderMid (rules : Array (Nat × Nat)) (xs : Array Nat) : Option Nat :=
  let sorted := xs.qsort (fun a b => rules.contains (a, b))
  if xs == sorted
    then none
    else findMid sorted
  
def part2 (input : Array String) :=
  let (rules, prints) := parse input
  let res :=
    prints
      |>.filterMap (reorderMid rules)
      |>.sum
  println! s!"{res}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D5
