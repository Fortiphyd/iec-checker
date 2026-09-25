open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

(* A physical output is written by assigning to its direct address, to a
   variable located at it, or through an output parameter ([OUT => %QW1]).
   Writes are counted along each path through the POU body, see
   {!Once_per_cycle}. *)

let is_output dv =
  match S.DirVar.get_loc dv with Some S.DirVar.LocQ -> true | _ -> false

let located_outputs decls =
  List.filter_map decls ~f:(fun d ->
      match S.VarDecl.get_located_at d with
      | Some dv when is_output dv -> Some (S.VarDecl.get_var_name d, S.DirVar.get_name dv)
      | _ -> None)
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

(** Output variables declared in configurations, resources and VAR_GLOBAL
    blocks, by name. *)
let global_outputs elements =
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) ->
        c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
      | e ->
        List.filter (AU.get_var_decls e) ~f:(fun d ->
            match S.VarDecl.get_attr d with Some (S.VarDecl.VarGlobal _) -> true | _ -> false))
  |> located_outputs

(** Output variables visible in a POU: its own, and globals it doesn't shadow
    with a variable of the same name. *)
let visible_outputs globals decls =
  let shadowed =
    List.filter_map decls ~f:(fun d ->
        match S.VarDecl.get_attr d with
        | Some (S.VarDecl.VarExternal _) -> None
        | _ -> Some (S.VarDecl.get_var_name d))
    |> String.Set.of_list
  in
  Map.merge_skewed
    (Map.filter_keys globals ~f:(fun n -> not (Set.mem shadowed n)))
    (located_outputs decls)
    ~combine:(fun ~key:_ _ local -> local)

(** The physical output written by assigning to [v], as a key identifying the
    output and a name to show. Elements with non-constant indexes are
    skipped, since which output they write isn't known. *)
let written_output outputs v =
  match S.VarUse.get_loc v with
  | S.VarUse.DirVar dv when is_output dv ->
    let addr = S.DirVar.get_name dv in
    Some (addr, addr)
  | S.VarUse.DirVar _ -> None
  | S.VarUse.SymVar sv ->
    let full = S.VarUse.get_name v in
    let base, member =
      match String.lsplit2 full ~on:'.' with
      | Some (b, m) -> b, "." ^ m
      | None -> full, ""
    in
    let indexes = S.SymVar.get_array_indexes sv in
    Option.bind (Map.find outputs base) ~f:(fun addr ->
        if List.exists indexes ~f:Option.is_none then None
        else
          let idx =
            if List.is_empty indexes then ""
            else Printf.sprintf "[%s]"
                (String.concat ~sep:", "
                   (List.filter_map indexes ~f:(Option.map ~f:Int.to_string)))
          in
          Some (addr ^ member ^ idx, Printf.sprintf "%s%s%s (%s)" base member idx addr))

let write_event outputs v =
  Option.map (written_output outputs v) ~f:(fun (key, shown) ->
      Once_per_cycle.{ key; shown; ti = S.VarUse.get_ti v })

let events outputs = function
  | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, lhs), (S.ASSIGN | S.ASSIGN_REF), _)) ->
    Option.to_list (write_event outputs lhs)
  | S.StmFuncCall (_, _, params) ->
    List.filter_map params ~f:(fun (p : S.func_param_assign) ->
        match p.stmt with
        | S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) -> write_event outputs v
        | _ -> None)
  | _ -> []

let check_elem globals elem =
  let outputs = visible_outputs globals (AU.get_var_decls elem) in
  Once_per_cycle.find_repeated ~events:(events outputs) (AU.get_top_stmts elem)
  |> List.map ~f:(fun (r : Once_per_cycle.repeat) ->
      let where =
        if r.in_loop then "it is written in a loop"
        else Printf.sprintf "it is already written on line %d" r.first.linenr
      in
      let msg =
        Printf.sprintf "Physical output %s is written more than once per PLC cycle: %s"
          r.event.shown where
      in
      Warn.mk r.event.ti.linenr r.event.ti.col "PLCOPEN-CP12" msg)

let do_check elements =
  let globals = global_outputs elements in
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem globals e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP12";
  name = "Physical outputs shall be written once per PLC cycle";
  summary =
    "Writing the same output twice in one cycle makes its value depend on \
     statement order.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP12";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
