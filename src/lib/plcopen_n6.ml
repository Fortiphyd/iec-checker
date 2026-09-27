open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Report names of tasks, POUs, types and variables shorter than the minimum
   or longer than the maximum length. Local variables of POUs (VAR and
   VAR_TEMP) have their own minimum if one is configured; the rule proposes
   8 characters, or 3 for local names. Loop counters are exempt from the
   minimum, as the rule allows. Struct members aren't checked. *)

let chars n = if n = 1 then "1 char" else Printf.sprintf "%d chars" n

let check_length ~min_len ~max_len name linenr col =
  let len = String.length name in
  if min_len > 0 && len < min_len then
    let msg = Printf.sprintf
        "Identifier %s is too short (%s, minimum %d)"
        name (chars len) min_len
    in
    Some (Warn.mk_for_name ~name linenr col "PLCOPEN-N6" msg)
  else if max_len > 0 && len > max_len then
    let msg = Printf.sprintf
        "Identifier %s is too long (%s, maximum %d)"
        name (chars len) max_len
    in
    Some (Warn.mk_for_name ~name linenr col "PLCOPEN-N6" msg)
  else None

(** Control variables of the FOR loops of a POU. *)
let loop_counters elem =
  let rec walk = function
    | S.StmFor (_, ctrl, body) ->
      (match ctrl.assign with
       | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, v), S.ASSIGN, _)) -> [S.VarUse.get_name v]
       | _ -> [])
      @ List.concat_map body ~f:walk
    | S.StmIf (_, _, body, elsifs, els) -> List.concat_map (body @ elsifs @ els) ~f:walk
    | S.StmElsif (_, _, body) | S.StmWhile (_, _, body) | S.StmRepeat (_, body, _) ->
      List.concat_map body ~f:walk
    | S.StmCase (_, _, sels, els) ->
      List.concat_map sels ~f:(fun (sel : S.case_selection) -> List.concat_map sel.body ~f:walk)
      @ List.concat_map els ~f:walk
    | S.StmExpr _ | S.StmFuncCall _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _
    | S.StmEmpty _ -> []
  in
  List.concat_map (AU.get_top_stmts elem) ~f:walk |> String.Set.of_list

let is_local d =
  match S.VarDecl.get_attr d with
  | Some (S.VarDecl.Var _ | S.VarDecl.VarTemp) | None -> true
  | _ -> false

let check_elem cfg elem =
  let min_len = cfg.Config.naming_min_length and max_len = cfg.Config.naming_max_length in
  let min_local = if cfg.naming_min_length_local > 0 then cfg.naming_min_length_local else min_len in
  let counters = loop_counters elem in
  let in_pou = match elem with S.IECConfiguration _ -> false | _ -> true in
  let var_warns =
    Naming.var_decls elem
    |> List.filter_map ~f:(fun d ->
        let name = S.VarDecl.get_var_name d in
        let ti = S.VarDecl.get_var_ti d in
        let local = in_pou && is_local d in
        let min_len =
          if local && Set.mem counters name then 0
          else if local then min_local
          else min_len
        in
        check_length ~min_len ~max_len (Naming.shown ti name) ti.linenr ti.col)
  in
  let name_warns =
    match elem with
    | S.IECConfiguration (_, c) ->
      List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
          List.filter_map r.tasks ~f:(fun t ->
              let ti = S.Task.get_ti t in
              check_length ~min_len ~max_len (Naming.shown ti (S.Task.get_name t)) ti.linenr ti.col))
    | e ->
      Option.bind (S.get_pou_name_as_written e) ~f:(fun (name, ti) ->
          check_length ~min_len ~max_len name ti.linenr ti.col)
      |> Option.to_list
  in
  var_warns @ name_warns

let do_check elems =
  let cfg = Config.get () in
  if cfg.naming_min_length <= 0 && cfg.naming_min_length_local <= 0 && cfg.naming_max_length <= 0
  then []
  else
    List.concat_map elems ~f:(check_elem cfg)
    |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
        Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-N6";
  name = "Define an acceptable name length";
  summary =
    "Identifiers shorter than the configured minimum or longer than the \
     configured maximum should be renamed.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N6";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
