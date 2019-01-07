(*****************************)
(*****************************)
(****                     ****)
(****   Typing contexts   ****)
(****                     ****)
(*****************************)
(*****************************)

Require Import List.
Require Import Main.SystemF.FreeVar.
Require Import Main.SystemF.LocalClosure.
Require Import Main.SystemF.Name.
Require Import Main.SystemF.Substitution.
Require Import Main.SystemF.Syntax.

Import ListNotations.

Inductive context :=
| cEmpty : context
| cEExtend : context -> name -> type -> context
| cTExtend : context -> name -> context.

(*******************)
(* Variable lookup *)
(*******************)

Fixpoint eLookup c1 x1 :=
  match c1 with
  | cEmpty => None
  | cEExtend c2 x2 t => if nameEq x1 x2 then Some t else eLookup c2 x1
  | cTExtend c2 _ => eLookup c2 x1
  end.

Fixpoint tLookup c1 x1 :=
  match c1 with
  | cEmpty => false
  | cEExtend c2 _ _ => tLookup c2 x1
  | cTExtend c2 x2 => if nameEq x1 x2 then true else tLookup c2 x1
  end.

(***********)
(* Domains *)
(***********)

Fixpoint eDomain c1 :=
  match c1 with
  | cEmpty => []
  | cEExtend c2 x t => x :: eDomain c2
  | cTExtend c2 _ => eDomain c2
  end.

Fixpoint tDomain c1 :=
  match c1 with
  | cEmpty => []
  | cEExtend c2 _ _ => tDomain c2
  | cTExtend c2 x => x :: tDomain c2
  end.

(************************)
(* Type well-formedness *)
(************************)

Definition tWellFormed c t :=
  tLocallyClosed t 0 /\
  incl (tFreeVars t) (tDomain c).

(***************************)
(* Context well-formedness *)
(***************************)

Inductive cWellFormed : context -> Prop :=
| cwfEmpty :
  cWellFormed cEmpty
| cwfEExtend :
  forall c t x,
  ~ In x (eDomain c) ->
  tWellFormed c t ->
  cWellFormed c ->
  cWellFormed (cEExtend c x t)
| cwfTExtend :
  forall c x,
  ~ In x (tDomain c) ->
  cWellFormed c ->
  cWellFormed (cTExtend c x).

Hint Constructors cWellFormed.

(*****************************)
(* Concatenation of contexts *)
(*****************************)

Fixpoint cConcat c1 c2 :=
  match c2 with
  | cEmpty => c1
  | cEExtend c3 x t2 => cEExtend (cConcat c1 c3) x t2
  | cTExtend c3 x => cTExtend (cConcat c1 c3) x
  end.

(****************************)
(* Substitution on contexts *)
(****************************)

Fixpoint cSub c1 x1 t1 :=
  match c1 with
  | cEmpty => cEmpty
  | cEExtend c2 x2 t2 => cEExtend (cSub c2 x1 t1) x2 (ttSub t2 x1 t1)
  | cTExtend c2 x2 => cTExtend (cSub c2 x1 t1) x2
  end.
