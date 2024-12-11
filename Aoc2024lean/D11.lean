import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet

namespace D11

open Std

partial def countDigitsAux (acc : Nat) (n : Nat) : Nat :=
  if n = 0
    then acc
    else countDigitsAux (acc + 1) (n / 10)

def countDigits (n : Nat) : Nat := countDigitsAux 0 n

-- #eval countDigits 2
-- #eval countDigits 233
-- #eval countDigits 29932

def splitStone (n : Nat) (halfDigits : Nat) : List Nat :=
  let fac := Nat.pow 10 halfDigits
  let (left, right) := (n / fac, n % fac)
  [left, right]

-- #eval splitStone 2345 2
-- #eval splitStone 230045 3

def rule (n : Nat) : List Nat :=
  if n == 0 then
    [1]
  else
    let digits := countDigits n
    if digits % 2 == 0 then
      splitStone n (digits / 2)
    else
      [n * 2024]

-- #eval [0, 1, 10, 99, 999].flatMap rule
-- #eval [125, 17].flatMap rule

def parse (s : String) : List Nat :=
  s.splitOn |>.map String.toNat!

def part1 (input : Array String) := do
  let mut stones := parse input[0]!
  for _ in [:25] do
    stones := stones.flatMap rule
  println! s!"{stones.length}"

def splitStone' (n : Nat) (halfDigits : Nat) : Nat × Nat :=
  let fac := Nat.pow 10 halfDigits
  (n / fac, n % fac)

partial def solve (mem : HashMap (Nat × Nat) Nat) (n : Nat) (step : Nat) : Nat × HashMap (Nat × Nat) Nat :=
  if step == 0 then
    (1, mem)
  else
    match mem[(n, step)]? with
    | .some items =>
      (items, mem)
    | .none =>
      if n == 0 then
        let (items, mem) := solve mem 1 (step - 1)
        let mem := mem.insert (0, step) items
        (items, mem)
      else
        let digits := countDigits n
        if digits % 2 == 0 then
          let (left, right) := splitStone' n (digits / 2)
          let (items_left, mem) := solve mem left (step - 1)
          let (items_right, mem) := solve mem right (step - 1)
          let items := items_left + items_right
          let mem := mem.insert (n, step) items
          (items, mem)
        else
          let (items, mem) := solve mem (n * 2024) (step - 1)
          let mem := mem.insert (n, step) items
          (items, mem)

def part2 (input : Array String) := do
  let mut stones := parse input[0]!
  let (count, _) := stones.foldl
    (fun acc stone =>
      let (count, mem) := acc
      let (items, mem) := solve mem stone 75
      (count + items, mem))
    (0, HashMap.empty)
  println! s!"{count}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D11
