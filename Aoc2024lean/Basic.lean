import Std.Internal.Parsec

namespace Parsec

open Std.Internal
open Parsec
open String

def parse! [Inhabited α] (p: Parser α) (s: String) : α :=
  match Parser.run p s with
  | .ok a => a
  | .error e => panic s!"Parsing failureon {s}: {e}"

def int : Parser Int :=
  tryCatch (pchar '-')
    (fun _ => Int.neg <$> digits)
    (fun () => digits)

end Parsec

namespace Array

def splitWhile (p : α → Bool) (xs: Array α) : Array α × Array α :=
  let head := xs.takeWhile p
  let tail := xs.extract head.size xs.size
  (head, tail)

def tail (xs : Array α) : Array α :=
  xs.extract 1 xs.size

def popFront? [Inhabited α] (xs : Array α) : Option (α × Array α) :=
  if xs.isEmpty
    then .none
    else .some (xs[0]!, xs.tail)

def popFront! [Inhabited α] (xs : Array α) : α × Array α :=
  (xs[0]!, xs.tail)

def sum [Add α] [OfNat α 0] (xs : Array α) : α :=
  xs.foldl (. + .) 0

def prod [Mul α] [OfNat α 1] (xs : Array α) : α :=
  xs.foldl (. * .) 1

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

namespace List

def dropRetLastAux (rev_acc : List α) : List α → Option (α × List α)
  | [] => .none
  | [x] => .some (x, rev_acc.reverse)
  | x :: xs => dropRetLastAux (x :: rev_acc) xs

def dropRetLast : List α → Option (α × List α) :=
  dropRetLastAux []

-- #eval dropRetLast ([] : List Nat)
-- #eval dropRetLast ([1] : List Nat)
-- #eval dropRetLast ([1, 2, 3] : List Nat)

end List
