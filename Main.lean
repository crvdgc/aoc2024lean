import Aoc2024lean


def main (args : List String) : IO Unit := do
  let args := args |>.drop 1 |>.take 2
  let (day, part) := (args[0]!, args[1]!)
  let input ← IO.FS.lines s!"input/d{day}-{part}"
  match day with
  | "1" => D1.run part input
  | day => println! s!"Day {day} not implemented"
