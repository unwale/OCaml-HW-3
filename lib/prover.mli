(** Copyright 2025-2025, Unwale *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser

type tactic =
  | EquivIntro
  | Intros
  | NotIntros
  | Axiom
  | Apply
  | AndElim
  | Contradiction
  | Qed

type proof_state = {
  goal : formula;
  context : formula list;
  remaining_goals : formula list;
}

val apply_tactic : proof_state -> tactic -> (proof_state, string) result
