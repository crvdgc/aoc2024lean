import Aoc2024lean

-- def isSafeDec xs :=
--   let notInRangeDec (x : Int) := x < 1 || x > 3
--   let diffs := dbgTraceVal (List.zipWith Int.sub xs xs.tail) |> List.toArray
--   let n := diffs.size
--   Id.run do
--     let ys := dbgTrace "here" ( fun () => diffs.map notInRangeDec)
--     let mut i := 0
--     let mut skipped := ys[0]! != ys[0]!
--     while i < n do
--       i := dbgTraceVal i
--       if notInRangeDec diffs[i]!
--         then if skipped
--           then return Bool.false
--           else
--             if i = 0
--               then skipped := Bool.true
--               else 
--                 let skippedDiff := diffs[i - 1]! + diffs[i]!
--                 if notInRangeDec skippedDiff
--                   then return Bool.false
--                   else skipped := Bool.true
--       i := i + 1
--     return Bool.true

def test (_ : Unit) : IO Unit := do
  println! s!"ans"

def main (args : List String) : IO Unit := do
  let args := args |>.drop 1 |>.take 2
  let (day, part) := (args[0]!, args[1]!)
  let input ← IO.FS.lines s!"input/d{day}-{part}"
  match day with
  | "1" => D1.run part input
  | "2" => D2.run part input
  | "3" => D3.run part input
  | "4" => D4.run part input
  | "5" => D5.run part input
  | "6" => D6.run part input
  | "7" => D7.run part input
  | "8" => D8.run part input
  | "test" => test ()
  | day => println! s!"Day {day} not implemented"
