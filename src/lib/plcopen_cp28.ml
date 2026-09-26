open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

module T = IECCheckerAnalysis.Expr_type

(** Whether [e] is a duration or a point in time, which change continuously
    and are unlikely to be exactly equal to a value. Dates are not included:
    comparing them for equality is meaningful. *)
let is_time env e =
  match T.type_of env e with
  | T.Elem (S.TIME | S.LTIME | S.TIME_OF_DAY | S.TOD | S.LTOD
           | S.DATE_AND_TIME | S.DT | S.LDATE_AND_TIME | S.LDT) -> true
  | _ -> false

let check_elem elements elem =
  let env = T.env_of elements elem in
  (* Comparisons can be nested in other expressions. Call arguments are
     separate expressions in [get_pou_exprs]. *)
  let rec check acc = function
    | S.ExprBin (ti, lhs, op, rhs) ->
      let acc = check (check acc lhs) rhs in
      begin match op with
        | S.EQ | S.NEQ when is_time env lhs || is_time env rhs ->
          let msg = "Time and physical measures comparisons shall not be equality or inequality" in
          Warn.mk_at ti "PLCOPEN-CP28" msg :: acc
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
  id = "PLCOPEN-CP28";
  name = "Time and physical measures comparisons shall not be equality or inequality";
  summary =
    "Use range comparisons instead of [=] / [<>] when comparing [TIME] values.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP28";
  severity = IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
