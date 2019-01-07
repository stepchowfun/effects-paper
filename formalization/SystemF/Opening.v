(******************************)
(******************************)
(****                      ****)
(****   Variable opening   ****)
(****                      ****)
(******************************)
(******************************)

Require Import Main.SystemF.Syntax.
Require Import Peano_dec.

Fixpoint eeOpen e1 i1 e2 :=
  match e1 with
  | eFreeVar _ => e1
  | eBoundVar i2 => if eq_nat_dec i1 i2 then e2 else e1
  | eAbs t e3 => eAbs t (eeOpen e3 (S i1) e2)
  | eApp e3 e4 => eApp (eeOpen e3 i1 e2) (eeOpen e4 i1 e2)
  | eTAbs e3 => eTAbs (eeOpen e3 i1 e2)
  | eTApp e3 t => eTApp (eeOpen e3 i1 e2) t
  end.

Fixpoint ttOpen t1 i1 t2 :=
  match t1 with
  | tFreeVar _ => t1
  | tBoundVar i2 => if eq_nat_dec i1 i2 then t2 else t1
  | tArrow t3 t4 => tArrow (ttOpen t3 i1 t2) (ttOpen t4 i1 t2)
  | tForAll t3 => tForAll (ttOpen t3 (S i1) t2)
  end.

Fixpoint etOpen e1 i1 t1 :=
  match e1 with
  | eFreeVar _ => e1
  | eBoundVar _ => e1
  | eAbs t2 e2 => eAbs (ttOpen t2 i1 t1) (etOpen e2 i1 t1)
  | eApp e2 e3 => eApp (etOpen e2 i1 t1) (etOpen e3 i1 t1)
  | eTAbs e2 => eTAbs (etOpen e2 (S i1) t1)
  | eTApp e2 t2 => eTApp (etOpen e2 i1 t1) (ttOpen t2 i1 t1)
  end.
