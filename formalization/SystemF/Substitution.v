(**************************)
(**************************)
(****                  ****)
(****   Substitution   ****)
(****                  ****)
(**************************)
(**************************)

Require Import Main.SystemF.Name.
Require Import Main.SystemF.Syntax.

Fixpoint eeSub e1 x1 e2 :=
  match e1 with
  | eFreeVar x2 => if nameEq x1 x2 then e2 else e1
  | eBoundVar _ => e1
  | eAbs t e3 => eAbs t (eeSub e3 x1 e2)
  | eApp e3 e4 => eApp (eeSub e3 x1 e2) (eeSub e4 x1 e2)
  | eTAbs e3 => eTAbs (eeSub e3 x1 e2)
  | eTApp e3 t => eTApp (eeSub e3 x1 e2) t
  end.

Fixpoint ttSub t1 x1 t2 :=
  match t1 with
  | tFreeVar x2 => if nameEq x1 x2 then t2 else t1
  | tBoundVar _ => t1
  | tArrow t3 t4 => tArrow (ttSub t3 x1 t2) (ttSub t4 x1 t2)
  | tForAll t3 => tForAll (ttSub t3 x1 t2)
  end.

Fixpoint etSub e1 x1 t1 :=
  match e1 with
  | eFreeVar _ => e1
  | eBoundVar _ => e1
  | eAbs t2 e2 => eAbs (ttSub t2 x1 t1) (etSub e2 x1 t1)
  | eApp e2 e3 => eApp (etSub e2 x1 t1) (etSub e3 x1 t1)
  | eTAbs e2 => eTAbs (etSub e2 x1 t1)
  | eTApp e2 t2 => eTApp (etSub e2 x1 t1) (ttSub t2 x1 t1)
  end.
