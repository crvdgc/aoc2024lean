import Aoc2024lean.Basic

namespace D6

def dirs : Array (Int × Int) := [(-1, 0), (0, 1), (1, 0), (0, -1)].toArray

def parse (input : Array String) : Nat × Array (Int × Int) × (Int × Int) := Id.run do
  let mut startPos : Int × Int := (0, 0)
  let mut obstacles : Array (Int × Int) := Array.empty
  let n : Nat := input[0]!.length
  for (s, i) in input.zipWithIndex do
    for c in s.toList, j in [0:n] do
      if c == '^'
        then startPos := (i, j)
        else
          if c == '#'
            then obstacles := obstacles.push (i, j)
  pure (n, obstacles, startPos)

def part1 (input : Array String) :=
  let m := input.size
  let (n, obstacles, startPos) := parse input
  -- println! s!"{n}, {obstacles}, {startPos}"
  let count := Id.run do
    let mut map := Array.mkArray m (Array.mkArray n false)
    let mut pos := startPos
    let mut dir : Nat := 0
    while 0 ≤ pos.fst && pos.fst < m && 0 ≤ pos.snd && pos.snd < n do
      map := map.modify pos.fst.toNat (fun r => r.set! pos.snd.toNat true)
      let (dx, dy) := dirs[dir]!
      let (x, y) := (pos.fst + dx, pos.snd + dy)
      if obstacles.contains (x, y)
        then dir := (dir + 1) % 4
        else pos := (x, y)
    map.map Array.count |>.sum
  println! s!"{count}"
  
def part2 (input : Array String) :=
  println! s!"{input}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D6
