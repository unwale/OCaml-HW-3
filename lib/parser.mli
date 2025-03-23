(** Copyright 2025-2025, Unwale *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type formula =
  | Var of string
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Equiv of formula * formula

type input = char list
type 'a parser_result = Failed | Parsed of 'a * input
type 'a parser = input -> 'a parser_result

val variable : formula parser
val conj : formula parser
val atom : formula parser
val formula : formula parser
val parse_formula : string -> formula parser_result
