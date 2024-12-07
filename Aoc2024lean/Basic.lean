import Std.Internal.Parsec

namespace Parsec

open Std.Internal
open Parsec
open String

def parse! [Inhabited α] (p: Parser α) (s: String) : α :=
  match Parser.run p s with
  | .ok a => a
  | .error e => panic s!"Parsing failureon {s}: {e}"

end Parsec

namespace Array

def splitWhile (p : α → Bool) (xs: Array α) : Array α × Array α :=
  let head := xs.takeWhile p
  let tail := xs.extract head.size xs.size
  (head, tail)

def tail (xs : Array α) : Array α :=
  xs.extract 1 xs.size

def sum [Add α] [OfNat α 0] (xs : Array α) : α :=
  xs.foldl (. + .) 0

def count (xs : Array Bool) : Nat :=
  xs.filter id |>.size

end Array
