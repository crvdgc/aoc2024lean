import Aoc2024lean.Basic

namespace D7

def parse (input : Array String) : Array (Int × Array Int) :=
  input.map <| fun line =>
    let xs := line.splitOn ":"
    let test := xs[0]!.toInt!
    let vs := xs[1]!.trim.splitOn |>.map String.toInt! |>.toArray
    (test, vs)

-- isMul
def ops (n : Nat) : List (Array Bool) :=
  match n with
  | 0 => [Array.empty]
  | n + 1 =>
    let res := ops n
    res.map (fun a => a.push true) ++ res.map (fun a => a.push false)

def getValid (test : Int) (vs : Array Int) : Option Int :=
  let isValid :=
    if vs.size == 0
      then false
      else
        -- add trailing + 0
        let ops := ops (vs.size - 1) |>.map (fun a => a.push false)
        let vs := vs.push 0
        let f (acc : Int) (p : Int × Bool) :=
          let (v, isMul) := p
          if isMul
            then (acc * v)
            else (acc + v)
        ops.any <| fun ops =>
          vs.tail.zip ops
            |>.foldl f vs[0]!
            |> (. = test)
  if isValid
    then .some test
    else .none

def part1 (input : Array String) :=
  let res := input
    |> parse
    |>.filterMap (Function.uncurry getValid)
    |>.sum
  println! s!"{res}"

partial def facAux (acc : Int) (n : Int) :=
  if n = 0
    then acc
    else
      facAux (acc * 10) (n / 10)

def fac (n : Int) : Int :=
  facAux 1 n

-- #eval fac 1
-- #eval fac 43
-- #eval fac 1325

def concat (m : Int) (n : Int) : Int :=
  m * fac n + n

-- #eval concat 123 345

def ops' (n : Nat) : List (Array (Int → Int → Int)) :=
  match n with
  | 0 => [Array.empty]
  | n + 1 =>
    let res := ops' n
    res.map (fun a => a.push (. + .)) ++
    res.map (fun a => a.push (. * .)) ++
    res.map (fun a => a.push concat)

def getValid' (test : Int) (vs : Array Int) : Option Int :=
  let isValid :=
    if vs.size == 0
      then false
      else
        -- add trailing + 0
        let ops := ops' (vs.size - 1) |>.map (fun a => a.push (. + .))
        let vs := vs.push 0
        let f (acc : Int) (p : Int × (Int → Int → Int)) :=
          let (v, op) := p
          op acc v
        ops.any <| fun ops =>
          vs.tail.zip ops
            |>.foldl f vs[0]!
            |> (. = test)
  if isValid
    then .some test
    else .none

def part2 (input : Array String) :=
  let res := input
    |> parse
    |>.filterMap (Function.uncurry getValid')
    |>.sum
  println! s!"{res}"

-- #eval (MLList.ofList [1]) |> isEmpty'

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D7
