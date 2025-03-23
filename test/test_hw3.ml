open Alcotest
open Hw3.Parser

let equal_result r1 r2 =
  match (r1, r2) with
  | Failed, Failed -> true
  | Parsed (Var s1, r1), Parsed (Var s2, r2) -> s1 = s2 && r1 = r2
  | Parsed (Not f1, r1), Parsed (Not f2, r2) -> f1 = f2 && r1 = r2
  | Parsed (And (f1, f2), r1), Parsed (And (f3, f4), r2) ->
      f1 = f3 && f2 = f4 && r1 = r2
  | Parsed (Or (f1, f2), r1), Parsed (Or (f3, f4), r2) ->
      f1 = f3 && f2 = f4 && r1 = r2
  | Parsed (Implies (f1, f2), r1), Parsed (Implies (f3, f4), r2) ->
      f1 = f3 && f2 = f4 && r1 = r2
  | Parsed (Equiv (f1, f2), r1), Parsed (Equiv (f3, f4), r2) ->
      f1 = f3 && f2 = f4 && r1 = r2
  | _ -> false

let rec pp_formula ppf = function
  | Var s -> Format.fprintf ppf "Var %s" s
  | Not f -> Format.fprintf ppf "Not (%a)" pp_formula f
  | And (f1, f2) ->
      Format.fprintf ppf "And (%a, %a)" pp_formula f1 pp_formula f2
  | Or (f1, f2) -> Format.fprintf ppf "Or (%a, %a)" pp_formula f1 pp_formula f2
  | Implies (f1, f2) ->
      Format.fprintf ppf "Implies (%a, %a)" pp_formula f1 pp_formula f2
  | Equiv (f1, f2) ->
      Format.fprintf ppf "Equiv (%a, %a)" pp_formula f1 pp_formula f2

let pp_result ppf = function
  | Failed -> Format.fprintf ppf "Failed"
  | Parsed (v, rest) ->
      Format.fprintf ppf "Parsed (%a, [%s])" pp_formula v
        (String.concat "; " (List.map (String.make 1) rest))

let result = testable pp_result equal_result

let test_variable () =
  check result "parse 'abc'"
    (Parsed (Var "abc", [ ' '; 'd' ]))
    (variable [ 'a'; 'b'; 'c'; ' '; 'd' ]);
  check result "parse 'PQ'" (Parsed (Var "PQ", [])) (variable [ 'P'; 'Q' ]);
  check result "fail on no letters" Failed (variable [ ' '; 'x' ])

let test_formula () =
  check result "parse variable 'a'" (Parsed (Var "a", [])) (parse_formula "a");
  check result "parse conjunction 'a/\\b'"
    (Parsed (And (Var "a", Var "b"), []))
    (parse_formula "a/\\b");
  check result "parse not '~a'"
    (Parsed (Not (Var "a"), []))
    (parse_formula "~a");
  check result "parse complex '~(a/\\b)->c->d'"
    (Parsed
       (Implies (Not (And (Var "a", Var "b")), Implies (Var "c", Var "d")), []))
    (parse_formula "~(a/\\b)->c->d");
  check result "parse complex with equiv 'a/\\b<->c->d'"
    (Parsed (Equiv (And (Var "a", Var "b"), Implies (Var "c", Var "d")), []))
    (parse_formula "a/\\b<->c->d");
  check result "fail on invalid '~)'" Failed (parse_formula "~)")

let () =
  run "Formula Parser Tests"
    [
      ("variable", [ test_case "variable parser" `Quick test_variable ]);
      ("formula", [ test_case "formula parser" `Quick test_formula ]);
    ]
