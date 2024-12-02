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

def sort [Ord α] (xs : Array α) : Array α :=
  Array.insertionSort xs (fun x₁ x₂ => Ord.compare x₁ x₂ = Ordering.lt)

end Array
