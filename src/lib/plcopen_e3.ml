open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(* Report <, >, <= and >= on pointers, references and addresses. Their order
   depends on how the PLC lays out memory; only = and <> are allowed.
   Comparing the values they point to, as in [x^ < y^], is fine. *)

let check_elem elements elem =
  let ptrs = Pointers.of_pou elements elem in
  let rec check acc = function
    | S.ExprBin (ti, a, op, b) ->
      let acc = check (check acc a) b in
      begin match op with
        | S.LT | S.GT | S.LE | S.GE
          when Pointers.is_address ptrs a || Pointers.is_address ptrs b ->
          Warn.mk_at ti "PLCOPEN-E3"
            (Printf.sprintf "Pointers and references shall only be compared with = and <>, \
                             not %s"
               (match op with S.LT -> "<" | S.GT -> ">" | S.LE -> "<=" | _ -> ">="))
          :: acc
        | _ -> acc
      end
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
  id = "PLCOPEN-E3";
  name = "Some comparator instructions shall not be used for pointer or reference manipulation";
  summary = "Pointers and references should only be compared with = and <>.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-E3";
  severity = Warn.Medium;
  plcopen_importance = Some Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
