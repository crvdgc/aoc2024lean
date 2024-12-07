import Aoc2024lean.Basic
import Batteries.Data.MLList

namespace D7

open MLList

abbrev L := MLList Id

instance [ToString α]: ToString (L α) where
  toString vs := vs.force.run.toString

def parse (input : Array String) : Array (Int × List Int) :=
  input.map <| fun line =>
    let xs := line.splitOn ":"
    let test := xs[0]!.toInt!
    let vs := xs[1]!.trim.splitOn |>.map String.toInt!
    (test, vs)

def isEmpty' (xs : L Int) : Bool :=
  xs.uncons.run.isNone

partial def allVs (vs : L Int) : L Int := do
  match vs.uncons.run with
  | .none => .nil
  | .some (v, vs) => do
    if isEmpty' vs
      then pure v
      else
        let isAdd ← MLList.ofList [true, false]
        let (v' : Int) ← allVs vs
        if isAdd
          then pure (v + v')
          else pure (v * v')

def has (test : Int) (vs : L Int) : Bool :=
  Id.run do
    for v in vs do
      if v == test then
        return true
    return false

def getValid (test : Int) (vs : List Int) : Option Int :=
  let isValid :=
    vs
      |> MLList.ofList
      |> allVs
      |> dbgTraceVal
      |> has test
  if (dbgTraceVal isValid)
    then .some test
    else .none

def part1 (input : Array String) :=
  let res := input
    |> parse
    |>.filterMap (Function.uncurry getValid)
    |>.sum
  println! s!"{res}"
  
def part2 (input : Array String) :=
  println! s!"{input}"

-- #eval (MLList.ofList [1]) |> isEmpty'

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D7
