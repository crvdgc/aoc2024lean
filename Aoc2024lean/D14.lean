import Aoc2024lean.Basic
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Internal.Parsec

namespace D14

open Std.Internal
open Parsec
open String
open Std

abbrev P := Int × Int

def parser : Parser (P × P) := do
  skipString "p="
  let x ← int
  skipString ","
  let y ← int
  skipString " v="
  let dx ← int
  skipString ","
  let dy ← int
  pure ((x, y), (dx, dy))

def simulate (steps : Int) (bound : P) (start : P) (speed : P) : P :=
  start
    |>.pairwiseBoth (. + .) (speed.both (. * steps))
    |>.map (. % bound.fst) (. % bound.snd)

def countQuadrant (halfBound : P) (acc : Array Int) (p : P) : Array Int :=
  if p.fst < halfBound.fst
    then
      if p.snd < halfBound.snd
        then acc.modify 0 (. + 1)
        else
          if p.snd > halfBound.snd
            then acc.modify 1 (. + 1)
            else acc
    else
      if p.fst > halfBound.fst
        then
          if p.snd < halfBound.snd
            then acc.modify 2 (. + 1)
            else
              if p.snd > halfBound.snd
                then acc.modify 3 (. + 1)
                else acc
        else acc

def part1 (bound : P) (input : Array String) := do
  let robots := input
    |>.map (parse! parser)
    |>.map (Function.uncurry <| simulate 100 bound)
    |>.foldl (countQuadrant <| bound.both (. / 2)) #[0, 0, 0, 0]
    |>.prod
  println! s!"{robots}"

def ppRobots (bound : P) (robots : Array P) : IO Unit := do
  let mut map := Array.mkArray bound.snd.toNat (Array.mkArray bound.fst.toNat '.')
  for robot in robots do
    map := map.modify robot.snd.toNat (fun row => row.set! robot.fst.toNat '#')
  for row in map do
    println! (row |>.map Char.toUInt8 |> ByteArray.mk |> String.fromUTF8!)

def part2 (bound : P) (input : Array String) := do
  let stdin ← IO.getStdin.toIO
  let mut (robots, speeds) := input.map (parse! parser) |>.unzip
  let mut i := 70
  robots := robots |>.zipWith speeds (simulate 70 bound)
  while true do
    println! s!"{i}"
    ppRobots bound robots
    let cmd ← stdin.getLine
    if cmd == "exit" then break
    i := i + 101
    robots := robots |>.zipWith speeds (simulate 101 bound)

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then
      let bound : P := if part == "1" then (101, 103) else (11, 7)
      part1 bound input
    else
      part2 (101, 103) input

end D14
