open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(* McCabe's cyclomatic complexity is the number of decisions plus one: each
   IF and ELSIF, each CASE selection and each loop decides which way the
   control flow goes. AND and OR aren't decisions: ST doesn't short-circuit
   them.

   PLCopen's rule CP9 gives McCabe values for two examples that are higher:
   12 and 8 where this counts 8 and 6. They come out when each AND and OR in
   a condition counts 1, a FOR loop 2 and each EXIT 1. The examples don't
   tell how to weigh anything else, which counts as in standard McCabe. They
   also allow AND and FOR to weigh 2 and 1, or 0 and 3; 1 and 2 is the
   reading closest to extended cyclomatic complexity, which counts boolean
   operators in conditions. *)

type weights = { bool_op : int; for_loop : int; exit : int }

let standard = { bool_op = 0; for_loop = 1; exit = 0 }
let plcopen = { bool_op = 1; for_loop = 2; exit = 1 }

let rec bool_ops = function
  | S.ExprBin (_, a, (S.AND | S.OR), b) -> 1 + bool_ops a + bool_ops b
  | S.ExprBin (_, a, _, b) -> bool_ops a + bool_ops b
  | S.ExprUn (_, _, e) -> bool_ops e
  | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> 0

let cond w = function
  | S.StmExpr (_, e) -> w.bool_op * bool_ops e
  | _ -> 0

let rec decisions w stmt =
  let sum = List.sum (module Int) ~f:(decisions w) in
  match stmt with
  | S.StmIf (_, c, body, elsifs, els) -> 1 + cond w c + sum body + sum elsifs + sum els
  | S.StmElsif (_, c, body) -> 1 + cond w c + sum body
  | S.StmCase (_, _, sels, els) ->
    List.length sels
    + List.sum (module Int) sels ~f:(fun (sel : S.case_selection) -> sum sel.body)
    + sum els
  | S.StmFor (_, _, body) -> w.for_loop + sum body
  | S.StmWhile (_, c, body) | S.StmRepeat (_, body, c) -> 1 + cond w c + sum body
  | S.StmExit _ -> w.exit
  | S.StmExpr _ | S.StmFuncCall _ | S.StmContinue _ | S.StmReturn _
  | S.StmEmpty _ -> 0

let weighted w elem =
  1 + List.sum (module Int) (AU.get_top_stmts elem) ~f:(decisions w)

let mccabe = weighted standard
let mccabe_plcopen = weighted plcopen

let rec count stmt =
  let sum = List.sum (module Int) ~f:count in
  match stmt with
  | S.StmIf (_, _, body, elsifs, els) -> 1 + sum body + sum elsifs + sum els
  | S.StmElsif (_, _, body) | S.StmFor (_, _, body) | S.StmWhile (_, _, body)
  | S.StmRepeat (_, body, _) -> 1 + sum body
  | S.StmCase (_, _, sels, els) ->
    1 + List.sum (module Int) sels ~f:(fun (sel : S.case_selection) -> sum sel.body) + sum els
  | S.StmEmpty _ -> 0
  | S.StmExpr _ | S.StmFuncCall _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> 1

let statements elem = List.sum (module Int) (AU.get_top_stmts elem) ~f:count
