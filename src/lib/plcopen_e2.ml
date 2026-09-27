open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(* Report arithmetic on pointers, references and addresses, such as
   [p := ADR(buf) + 2] or [p := p + SIZEOF(INT)]. The rule allows equality
   and inequality only; arrays and their indexes should be used to reach
   further data. A chain like [ADR(a) + 2 + 4] is reported once. *)

let check_elem elements elem =
  let ptrs = Pointers.of_pou elements elem in
  let rec check acc = function
    | S.ExprBin (ti, a, op, b) ->
      let acc = check (check acc a) b in
      let direct e =
        Pointers.is_address ptrs e
        && (match e with S.ExprBin (_, _, op, _) -> not (Pointers.is_arithmetic op) | _ -> true)
      in
      if Pointers.is_arithmetic op && (direct a || direct b) then
        Warn.mk_at ti "PLCOPEN-E2"
          "Pointer arithmetic shall not be used: use an array and its index to reach the data"
        :: acc
      else acc
    | S.ExprUn (_, _, e) -> check acc e
    | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> acc
  in
  AU.get_pou_exprs elem |> List.fold ~init:[] ~f:check |> List.rev

let do_check elements =
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        check_elem elements e
      | _ -> [])
  |> List.dedup_and_sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-E2";
  name = "Pointer arithmetic shall not be used";
  summary = "Addresses shouldn't be computed with arithmetic; use arrays.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-E2";
  severity = Warn.High;
  plcopen_importance = Some Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
