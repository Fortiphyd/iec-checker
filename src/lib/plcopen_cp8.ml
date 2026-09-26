open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

module T = IECCheckerAnalysis.Expr_type

(** Whether [e] is a REAL or LREAL value, based on declarations and literals
    (see {!IECCheckerAnalysis.Expr_type}). *)
let is_float env e =
  match T.type_of env e with
  | T.Elem (S.REAL | S.LREAL) | T.Real_literal -> true
  | _ -> false

let check_elem elements elem =
  let env = T.env_of elements elem in
  (* Comparisons can be nested in other expressions. Call arguments are
     separate expressions in [get_pou_exprs]. *)
  let rec check acc = function
    | S.ExprBin (ti, lhs, op, rhs) ->
      let acc = check (check acc lhs) rhs in
      begin match op with
        | S.EQ | S.NEQ when is_float env lhs || is_float env rhs ->
          let msg = "Floating point comparison shall not be equality or inequality" in
          Warn.mk ti.linenr ti.col "PLCOPEN-CP8" msg :: acc
        | _ -> acc
      end
    | S.ExprUn (_, _, e) -> check acc e
    | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> acc
  in
  AU.get_pou_exprs elem |> List.fold ~init:[] ~f:check |> List.rev

let do_check elems =
  List.fold_left
    ~init:[]
    elems
    ~f:(fun acc elem -> acc @ (check_elem elems elem))

let detector : Detector.t = {
  id = "PLCOPEN-CP8";
  name = "Floating point comparison shall not be equality or inequality";
  summary =
    "Use a tolerance instead of [=] or [<>] when comparing [REAL] values.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP8";
  severity = IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
