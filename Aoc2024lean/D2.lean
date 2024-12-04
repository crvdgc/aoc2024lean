namespace D2

def parseReport (s : String) : List Int :=
  s.splitOn |>.map String.toInt!

def isSafe (xs : List Int) : Bool :=
  match xs with
  | [] | [_] => .true
  | x :: y :: _ =>
    if x = y
      then .false
      else if x < y
        then allPairs (fun (x, y) => x + 1 ≤ y && y ≤ x + 3)
        else allPairs (fun (x, y) => x - 3 ≤ y && y ≤ x - 1)
  where
    allPairs p := List.zip xs xs.tail |>.all p

def part1 (input : Array String) : IO Unit := do
  let ans := input
    |>.map parseReport
    |>.foldl
      (fun acc report =>
        if isSafe report
          then acc + 1
          else acc)
      0
  println! s!"{ans}"

def isSafeRelaxed (xs : List Int) : Bool :=
  let notInRangeDec x := x < 1 || x > 3
  let isSafeDec xs :=
    let diffs := List.zipWith Int.sub xs xs.tail |> List.toArray
    let n := diffs.size
    Id.run do
      let mut skipped := false
      let mut i := 0
      while i < n do
        let cur := diffs[i]!
        if notInRangeDec cur
          then
            if skipped
              then
                -- can only skip once
                return false
              else 
                -- haven't skipped
                if i = 0
                  then
                    -- try skip after
                    if i = n - 1
                      then
                        -- only 2 elements
                        return true
                      else
                        if notInRangeDec (cur + diffs[i + 1]!)
                          then
                            -- must skip first
                            skipped := true
                          else
                            skipped := true
                            i := i + 1
                  else
                    if i = n - 1
                      then
                        -- skip last one and is safe
                        return true
                      else
                        -- can choose to skip before or after
                        -- if possible, we want to skip after
                        if notInRangeDec (cur + diffs[i + 1]!)
                          then
                            -- we have to skip before
                            if notInRangeDec (cur + diffs[i - 1]!)
                              then
                                -- no way to skip
                                return false
                              else
                                skipped := true
                          else
                            -- we skipped after
                            skipped := true
                            i := i + 1
        -- in range
        i := i + 1
      return true
  isSafeDec xs || isSafeDec (xs.map Int.neg)

def part2 (input : Array String) : IO Unit := do
  let ans := input
    |>.map parseReport
    -- |>.map isSafeRelaxed
    |>.foldl
      (fun acc report =>
        if isSafeRelaxed report
          then acc + 1
          else acc)
      0
  println! s!"{ans}"

def run (part : String) (input : Array String) : IO Unit := do
  match part with
  | "1" | "1e" => part1 input
  | "2" | "2e" => part2 input
  | _ => println! s!"{input}"

end D2
