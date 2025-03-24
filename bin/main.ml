(** Copyright 2025-2025, Unwale *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Hw3.Parser
open Hw3.Prover

let parse_tactic s =
  match String.sub (String.lowercase_ascii s) 0 2 with
  | "eq" -> EquivIntro
  | "in" -> Intros
  | "no" -> NotIntros
  | "ax" -> Axiom
  | "ap" -> Apply
  | "an" -> AndElim
  | "co" -> Contradiction
  | "qe" -> Qed
  | _ ->
      Printf.printf "unknown tactic: %s" s;
      exit 1

let init_proof_state s =
  match parse_formula s with
  | Parsed (f, _) -> { goal = f; context = []; remaining_goals = [] }
  | Failed ->
      Printf.printf " failed to parse formula\n";
      exit 1

let rec string_of_formula = function
  | Var v -> v
  | Not f -> "~" ^ string_of_formula f
  | And (f1, f2) ->
      Printf.sprintf "(%s /\\ %s)" (string_of_formula f1) (string_of_formula f2)
  | Or (f1, f2) ->
      Printf.sprintf "(%s \\/ %s)" (string_of_formula f1) (string_of_formula f2)
  | Implies (f1, f2) ->
      Printf.sprintf "(%s -> %s)" (string_of_formula f1) (string_of_formula f2)
  | Equiv (f1, f2) ->
      Printf.sprintf "(%s <-> %s)" (string_of_formula f1) (string_of_formula f2)

let print_state state =
  Printf.printf "goal: %s\n" (string_of_formula state.goal);
  Printf.printf "context: %s\n"
    (String.concat "; " (List.map string_of_formula state.context));

  let remaining_goals_count = List.length state.remaining_goals in
  if remaining_goals_count > 0 then
    Printf.printf "n remaining goals: %d\n" remaining_goals_count;

  print_newline ()

let rec proof_loop state =
  print_state state;
  Printf.printf "> ";
  let input = read_line () in
  match String.trim input with
  | "" -> proof_loop state
  | tactic_str -> (
      let tactic = parse_tactic tactic_str in
      match apply_tactic state tactic with
      | Ok new_state -> (
          match tactic with
          | Contradiction | Axiom -> proof_loop new_state
          | Qed -> Printf.printf "proof completed\n"
          | _ -> proof_loop new_state)
      | Error message -> Printf.printf "error: %s\n\n" message)

let () = proof_loop (init_proof_state Sys.argv.(1))
