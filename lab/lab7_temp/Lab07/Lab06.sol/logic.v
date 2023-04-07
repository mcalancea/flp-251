
Definition doubleNeg : Prop := forall a : Prop, ~ ~ a -> a.
Definition excludedMiddle : Prop := forall a : Prop, a \/ ~ a.

Lemma doubleNegFromExcludedMiddle : forall a : Prop, a \/ ~a -> (~ ~ a -> a).
Proof.
  intros a Hem Hnna.
  destruct Hem as [Ha | Hna]; [exact Ha |].
  exfalso.
  apply Hnna; exact Hna.
Qed.

Lemma excludedMiddleFromDoubleNeg : doubleNeg -> excludedMiddle.
Proof.
  intro doubleNeg.
  unfold excludedMiddle.
  intro a.
  apply doubleNeg.
  intro Hnor.
  assert (Hnna : ~ ~ a).
  {
    intro Hna.
    elim Hnor.
    right.
    assumption.
  }
  apply Hnna.
  intro Ha.
  elim Hnor.
  left.
  assumption.
Defined.

Print excludedMiddleFromDoubleNeg.
