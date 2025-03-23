open Alcotest
open Hw3.Parser
open Hw3.Prover

let rec formula_equal f1 f2 =
  match (f1, f2) with
  | Var s1, Var s2 -> s1 = s2
  | Not f1', Not f2' -> formula_equal f1' f2'
  | And (a1, b1), And (a2, b2)
  | Or (a1, b1), Or (a2, b2)
  | Implies (a1, b1), Implies (a2, b2)
  | Equiv (a1, b1), Equiv (a2, b2) ->
      formula_equal a1 a2 && formula_equal b1 b2
  | _ -> false

let apply_tactics state tactics =
  List.fold_left
    (fun state tactic ->
      match apply_tactic state tactic with
      | Ok new_state -> new_state
      | Error message -> failwith message)
    state tactics

let run_proof formula tactics =
  let ast =
    match parse_formula formula with
    | Parsed (f, _) -> f
    | Failed -> failwith "Failed to parse formula"
  in
  let initial_state = { goal = ast; context = []; remaining_goals = [] } in
  let final_state = apply_tactics initial_state tactics in
  check bool "Proof should succeed" true
    (formula_equal final_state.goal (Var "proved")
    && final_state.remaining_goals = [])

let test_proof_equiv () =
  let formula = "(a->b->c)<->(a/\\b->c)" in
  let tactics =
    [
      EquivIntro;
      Intros;
      Intros;
      AndElim;
      Apply;
      Apply;
      Axiom;
      Intros;
      Intros;
      Intros;
      Axiom;
      Qed;
    ]
  in
  let _ = run_proof formula tactics in
  ()

let test_proof_contradiction () =
  let formula = "(P/\\~P)->Q" in
  let tactics = [ Intros; AndElim; Contradiction; Qed ] in
  let _ = run_proof formula tactics in
  ()

let test_proof_double_negation_intro () =
  let formula = "P->~~P" in
  let tactics = [ Intros; NotIntros; Contradiction; Qed ] in
  let _ = run_proof formula tactics in
  ()

let test_proof_excluded_middle () =
  let formula = "(P\\/~P)->(~~P->P)" in
  let tactics = [ Intros; Intros; NotIntros; Contradiction; Qed ] in
  let _ = run_proof formula tactics in
  ()

let suite =
  [
    ("equivalence: (A -> B -> C) <-> (A /\\ B -> C)", `Quick, test_proof_equiv);
    ("contradiction: (P /\\ ~P) -> Q", `Quick, test_proof_contradiction);
    ("double negation: P -> ~~P", `Quick, test_proof_double_negation_intro);
    ( "excluded middle: (P \\/ ~P) -> (~~P -> P)",
      `Quick,
      test_proof_excluded_middle );
  ]

let () = Alcotest.run "Prover Tests" [ ("Proofs", suite) ]
