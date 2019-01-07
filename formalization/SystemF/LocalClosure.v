(**************************************)
(**************************************)
(****                              ****)
(****   Local closure predicates   ****)
(****                              ****)
(**************************************)
(**************************************)

Require Import Main.SystemF.Syntax.

Inductive tLocallyClosed : type -> nat -> Prop :=
| tlcFreeVar :
  forall n x,
  tLocallyClosed (tFreeVar x) n
| tlcBoundVar :
  forall n1 n2,
  n1 < n2 ->
  tLocallyClosed (tBoundVar n1) n2
| tlcArrow :
  forall n t1 t2,
  tLocallyClosed t1 n ->
  tLocallyClosed t2 n ->
  tLocallyClosed (tArrow t1 t2) n
| tlcForAll :
  forall n t,
  tLocallyClosed t (S n) ->
  tLocallyClosed (tForAll t) n.

Hint Constructors tLocallyClosed.

Inductive eLocallyClosed : term -> nat -> nat -> Prop :=
| elcFreeVar :
  forall ne nt x,
  eLocallyClosed (eFreeVar x) ne nt
| elcBoundVar :
  forall ne1 ne2 nt,
  ne1 < ne2 ->
  eLocallyClosed (eBoundVar ne1) ne2 nt
| elcAbs :
  forall e ne nt t,
  tLocallyClosed t nt ->
  eLocallyClosed e (S ne) nt ->
  eLocallyClosed (eAbs t e) ne nt
| elcApp :
  forall e1 e2 ne nt,
  eLocallyClosed e1 ne nt ->
  eLocallyClosed e2 ne nt ->
  eLocallyClosed (eApp e1 e2) ne nt
| elcTAbs :
  forall e ne nt,
  eLocallyClosed e ne (S nt) ->
  eLocallyClosed (eTAbs e) ne nt
| elcTApp :
  forall e ne nt t,
  eLocallyClosed e ne nt ->
  tLocallyClosed t nt ->
  eLocallyClosed (eTApp e t) ne nt.

Hint Constructors eLocallyClosed.
