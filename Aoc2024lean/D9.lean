import Aoc2024lean.Basic

namespace D9

inductive FS where
  | file : Nat → Nat → FS -- id, length
  | free : Nat → FS       -- length

instance : ToString FS where
  toString : FS → String
    | .file id length => String.mk (List.replicate length (Char.ofNat (id + '0'.toNat)))
    | .free length => String.mk (List.replicate length '.')

def FS.toStringList : List FS → String :=
  String.join ∘ List.map toString

mutual

def parseFile (fileId : Nat) (acc : List FS) : List Nat → List FS
  | [] => acc
  | x :: xs =>
    let file := FS.file fileId x
    parseFree (fileId + 1) (file :: acc) xs

def parseFree (fileId : Nat) (acc : List FS) : List Nat → List FS
  | [] => acc
  | x :: xs =>
    let free := FS.free x
    parseFile fileId (free :: acc) xs
end

-- return reversed FS
def parse (input : String) : List FS :=
  input.toList |>.map (Char.toNat . - '0'.toNat) |> parseFile 0 [] |>.reverse

def addFile (id : Nat) (pos : Nat) (length : Nat) : Nat × Nat :=
  let newPos := pos + length
  let sum := List.range' pos newPos
    |>.zipWith (. * .) (List.replicate length id)
    |>.sum
  (newPos, sum)

-- #eval addFile 2 0 3

partial def compress (pos : Nat) (checksum : Nat) : List FS → Nat
  | [] => checksum
  | .free length :: fs =>
    match fs.dropRetLast with
    | .none => checksum
    | .some (.free _, fs) =>
      compress pos checksum (.free length :: fs)
    | .some (.file id length', fs) =>
      if length' = length then
        let (newPos, curChecksum) := addFile id pos length
        compress newPos (checksum + curChecksum) fs
      else if length' > length then
        let frag := .file id (length' - length)
        let (newPos, curChecksum) := addFile id pos length
        compress newPos (checksum + curChecksum) (fs ++ [frag])
      else
        let frag := .free (length - length')
        let (newPos, curChecksum) := addFile id pos length'
        compress newPos (checksum + curChecksum) (frag :: fs)
  | .file id length :: fs =>
    let (newPos, curChecksum) := addFile id pos length
    compress newPos (checksum + curChecksum) fs

def part1 (input : Array String) :=
  let fs := parse input[0]!
  let checksum := compress 0 0 fs
  println! s!"{checksum}"

def part2 (input : Array String) :=
  println! s!"{input}"

def run (part : String) (input : Array String) : IO Unit :=
  if part.startsWith "1"
    then part1 input
    else part2 input

end D9
