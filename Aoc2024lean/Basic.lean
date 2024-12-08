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

namespace List

def tally (xs : List α) (p : α → Bool) : Nat :=
  xs.map (fun x => if p x then 1 else 0) |>.sum

end List

namespace Prod

def both (f : α → β) : (α × α) → (β × β) := Prod.map f f

def pairwise (f : α₁ → β₁ → γ₁) (g : α₂ → β₂ → γ₂) (p₁ : α₁ × α₂) (p₂ : β₁ × β₂) : γ₁ × γ₂ :=
  (f p₁.fst p₂.fst, g p₁.snd p₂.snd)

def pairwiseBoth (f : α → β → γ) := pairwise f f

end Prod
