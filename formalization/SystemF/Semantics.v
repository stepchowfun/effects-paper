(***********************)
(***********************)
(****               ****)
(****   Semantics   ****)
(****               ****)
(***********************)
(***********************)

Require Import Main.SystemF.LocalClosure.
Require Import Main.SystemF.Opening.
Require Import Main.SystemF.Syntax.

Inductive value : term -> Prop :=
| vAbs :
  forall e t,
  eLocallyClosed (eAbs t e) 0 0 ->
  value (eAbs t e)
| vTAbs :
  forall e,
  eLocallyClosed (eTAbs e) 0 0 ->
  value (eTAbs e).

Hint Constructors value.

Inductive step : term -> term -> Prop :=
| sBeta :
  forall e1 e2 t,
  eLocallyClosed (eAbs t e1) 0 0 ->
  value e2 ->
  step (eApp (eAbs t e1) e2) (eeOpen e1 0 e2)
| sApp1 :
  forall e1 e2 e3,
  eLocallyClosed e3 0 0 ->
  step e1 e2 ->
  step (eApp e1 e3) (eApp e2 e3)
| sApp2 :
  forall e1 e2 e3,
  value e1 ->
  step e2 e3 ->
  step (eApp e1 e2) (eApp e1 e3)
| sTApp :
  forall e1 e2 t,
  tLocallyClosed t 0 ->
  step e1 e2 ->
  step (eTApp e1 t) (eTApp e2 t)
| sTBeta :
  forall e1 t,
  eLocallyClosed (eTAbs e1) 0 0 ->
  tLocallyClosed t 0 ->
  step (eTApp (eTAbs e1) t) (etOpen e1 0 t).

Hint Constructors step.
