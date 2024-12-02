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

end Array
