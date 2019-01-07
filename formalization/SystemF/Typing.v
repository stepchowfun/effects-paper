(**************************)
(**************************)
(****                  ****)
(****   Typing rules   ****)
(****                  ****)
(**************************)
(**************************)

Require Import List.
Require Import Main.SystemF.Context.
Require Import Main.SystemF.Opening.
Require Import Main.SystemF.Syntax.

Inductive hasType : context -> term -> type -> Prop :=
| htFreeVar :
  forall c t x,
  cWellFormed c ->
  eLookup c x = Some t ->
  hasType c (eFreeVar x) t
| htAbs :
  forall c e l t1 t2,
  (
    forall x,
    ~ In x l ->
    hasType (cEExtend c x t2) (eeOpen e 0 (eFreeVar x)) t1
  ) ->
  hasType c (eAbs t2 e) (tArrow t2 t1)
| htApp :
  forall c e1 e2 t1 t2,
  hasType c e1 (tArrow t2 t1) ->
  hasType c e2 t2 ->
  hasType c (eApp e1 e2) t1
| htTAbs :
  forall c e l t,
  (
    forall x,
    ~ In x l ->
    hasType (cTExtend c x) (etOpen e 0 (tFreeVar x)) (ttOpen t 0 (tFreeVar x))
  ) ->
  hasType c (eTAbs e) (tForAll t)
| htTApp :
  forall c e t1 t2,
  tWellFormed c t2 ->
  hasType c e (tForAll t1) ->
  hasType c (eTApp e t2) (ttOpen t1 0 t2).

Hint Constructors hasType.
