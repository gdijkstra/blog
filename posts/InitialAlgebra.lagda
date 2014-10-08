* Motivate why one would want to study the semantics of inductive types.

* Start out with Natural numbers 

\begin{code}
open import Agda.Primitive public

Type : Set₁
Type = Set

data Nat : Type where
  Z : Nat
  S : Nat -> Nat
\end{code}

* Observe that we have two functions, so maybe we can look at it as a
  something like Σ Nat : Type . Nat × (Nat -> Nat). (Maybe not
  literally use Σ-types.)
* Define term *target*
* Observe that this is the same as Σ Nat : Type . (1 + Nat -> Nat).
* Notice that we have in : 1 + Nat -> Nat and out : Nat -> 1 + Nat functions and that it's an iso.
* Hence we find out that it is some (least) fixpoint of a X = 1 + X we're after.
* Having just a fixpoint doesn't give us a way to write recursive functions Nat -> X.
* Introduce algebras.
* Talk about computation rules and introduce algebra morphisms.
* Maybe talk about some details of how these equations should hold.
* Talk about initiality.
* Mention Lambek's lemma
* Mention strict positivity restrictions and such.
