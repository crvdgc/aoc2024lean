namespace D4

def split : Array String → Array (Array Char) :=
  Array.map (List.toArray ∘ String.toList)

def part1 (input: Array String) :=
  let a := split input
  let dirs :=
    let steps := [-1, 0, 1]
    steps.flatMap (steps.map ∘ Prod.mk) |>.filter (. != (0, 0))
  let word := ['M', 'A', 'S']
  let count := Id.run do
    let m := a.size
    let n := a[0]!.size
    let mut acc := 0
    for i in [:m] do
      for j in [:n] do
        if a[i]![j]! = 'X' then
          for (dx, dy) in dirs do
            let mut x := Int.ofNat i
            let mut y := Int.ofNat j
            let mut failed := false
            for c in word do
              x := x + dx
              y := y + dy
              if x < 0 || x ≥ m || y < 0 || y ≥ n then
                failed := true
                break
              else if a[x.toNat]![y.toNat]! != c then
                failed := true
                break
            if not failed then 
              acc := acc + 1
    return acc
  println! s!"{count}"

def part2 (input : Array String) :=
  let a := split input
  let count := Id.run do
    let m := a.size
    let n := a[0]!.size
    let inBoundEq (x : Int) (y : Int) c :=
      not (x < 0 || x ≥ m || y < 0 || y ≥ n) && a[x.toNat]![y.toNat]! = c
    let mut acc := 0
    for i in [:m] do
      for j in [:n] do
        if a[i]![j]! = 'A' then
          let (x, y) := (Int.ofNat i, Int.ofNat j)
          let cond₁ := (inBoundEq (x - 1) (y - 1) 'M' && inBoundEq (x + 1) (y + 1) 'S')
                        || (inBoundEq (x - 1) (y - 1) 'S' && inBoundEq (x + 1) (y + 1) 'M')
          let cond₂ := (inBoundEq (x - 1) (y + 1) 'M' && inBoundEq (x + 1) (y - 1) 'S')
                        || (inBoundEq (x - 1) (y + 1) 'S' && inBoundEq (x + 1) (y - 1) 'M')
          if cond₁ && cond₂ then
            acc := acc + 1
    return acc
  println! s!"{count}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input


end D4
