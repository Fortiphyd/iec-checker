open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(* Report dynamic memory allocation: the __NEW operator of CODESYS and
   TwinCAT, and the SysMemAlloc and SysMemRealloc functions of the CODESYS
   SysMem library. An allocator implemented by the application itself, which
   the rule forbids too, can't be told apart from other code. *)

let allocates name =
  String.equal name "__NEW"
  || String.is_prefix name ~prefix:"SYSMEMALLOC"
  || String.is_prefix name ~prefix:"SYSMEMREALLOC"

let calls elem =
  let rec in_expr = function
    | S.ExprFuncCall (_, S.StmFuncCall (_, f, _)) -> [f]
    | S.ExprBin (_, a, _, b) -> in_expr a @ in_expr b
    | S.ExprUn (_, _, a) -> in_expr a
    | S.ExprFuncCall _ | S.ExprVariable _ | S.ExprConstant _ -> []
  in
  List.filter_map (AU.get_pou_stmts elem) ~f:(function
      | S.StmFuncCall (_, f, _) -> Some f
      | _ -> None)
  @ List.concat_map (AU.get_pou_exprs elem) ~f:in_expr

let do_check elements =
  List.concat_map elements ~f:(fun e ->
      calls e
      |> List.filter ~f:(fun f -> allocates (S.Function.get_name f))
      |> List.map ~f:(fun f ->
          let ti = S.Function.get_ti f in
          let name = if String.is_empty ti.raw then S.Function.get_name f else ti.raw in
          Warn.mk_at ti "PLCOPEN-E1"
            (Printf.sprintf "Dynamic memory allocation shall not be used: %s allocates memory \
                             at run time" name)))
  |> List.dedup_and_sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-E1";
  name = "Dynamic memory allocation shall not be used";
  summary = "Memory shouldn't be allocated at run time.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-E1";
  severity = Warn.Medium;
  plcopen_importance = Some Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
