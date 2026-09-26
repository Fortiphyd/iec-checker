open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(* McCabe's cyclomatic complexity is the number of decisions plus one: each
   IF and ELSIF, each CASE selection and each loop decides which way the
   control flow goes. AND and OR aren't decisions: ST doesn't short-circuit
   them. *)

let rec decisions stmt =
  let sum = List.sum (module Int) ~f:decisions in
  match stmt with
  | S.StmIf (_, _, body, elsifs, els) -> 1 + sum body + sum elsifs + sum els
  | S.StmElsif (_, _, body) -> 1 + sum body
  | S.StmCase (_, _, sels, els) ->
    List.length sels
    + List.sum (module Int) sels ~f:(fun (sel : S.case_selection) -> sum sel.body)
    + sum els
  | S.StmFor (_, _, body) | S.StmWhile (_, _, body) | S.StmRepeat (_, body, _) -> 1 + sum body
  | S.StmExpr _ | S.StmFuncCall _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _
  | S.StmEmpty _ -> 0

let mccabe elem =
  1 + List.sum (module Int) (AU.get_top_stmts elem) ~f:decisions
