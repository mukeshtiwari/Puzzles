Require Import List.
Import ListNotations.
Require Import Lia.

Fixpoint eval_poly (x : nat) (cf : list nat) :=
  match cf with
  | [] => 0
  | an :: cf' => eval_poly x cf' + an * Nat.pow x (length cf')
  end.

Fixpoint horner_loop (acc : nat) (x : nat) (cf : list nat) :=
  match cf with
  | [] => acc
  | an :: cf' => horner_loop (acc * x + an) x cf'
  end.

Definition horner (x : nat) (cf : list nat) := horner_loop 0 x cf.

Lemma horner_gen : forall cf x acc,
    acc * Nat.pow x (length cf) + horner x cf = horner_loop acc x cf.
Proof.
  induction cf.
  + intros ? ?; simpl.
    unfold horner; cbn;
      lia.
  + intros ? ?; cbn.
    rewrite <- IHcf.
    rewrite <- IHcf.
    lia.
Qed.

    
Theorem horner_correct : forall x cf,
    eval_poly x cf = horner x cf.
Proof.
  intros ? ?. revert x.
  induction cf.
  +  intros ?. auto.
  +  intros ?. cbn.
     rewrite <- horner_gen, IHcf.
     lia.
Qed.
