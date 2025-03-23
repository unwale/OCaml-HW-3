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
  | _ -> failwith ("unknown tactic: " ^ s)

let init_proof_state s =
  match parse_formula s with
  | Parsed (f, _) -> { goal = f; context = []; remaining_goals = [] }
  | Failed -> failwith "failed to parse formula"

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

let main () =
  let formula_str = Sys.argv.(1) in
  let state = ref (init_proof_state formula_str) in
  Printf.printf "starting proof for: %s\n" formula_str;
  print_state !state;

  while true do
    Printf.printf "> ";
    let input = read_line () in
    match String.trim input with
    | "" -> ()
    | tactic_str -> (
        let tactic = parse_tactic tactic_str in
        match apply_tactic !state tactic with
        | Ok new_state ->
            state := new_state;
            print_state !state;
            if tactic = Qed then Printf.printf "proof completed\n %d" (1 / 0)
        | Error message -> Printf.printf "error: %s\n\n" message)
  done

let () = main ()
