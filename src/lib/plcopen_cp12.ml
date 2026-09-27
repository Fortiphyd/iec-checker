open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util
module PM = IECCheckerAnalysis.Program_model

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

(** Function block instances declared in a POU, with their types. *)
let instance_types decls =
  List.filter_map decls ~f:(fun d ->
      match S.VarDecl.get_ty_spec d with
      | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
      | _ -> None)
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

(** Outputs written by calling [inst], a function block of type [ty]. *)
let call_writes ~effects ~globals ~(ti : IECCheckerCore.Tok_info.t) inst ty =
  (effects ty).PM.writes
  |> List.filter_map ~f:(fun (w : PM.access) ->
      match w.target with
      | PM.Address a when String.is_prefix a ~prefix:"%Q" -> Some a
      | PM.Global (n, _) -> Map.find globals n
      | PM.Address _ -> None)
  |> List.dedup_and_sort ~compare:String.compare
  |> List.map ~f:(fun a ->
      Once_per_cycle.{ key = a; shown = Printf.sprintf "%s (written by %s)" a inst; ti })

let events ~effects ~globals ~instances outputs = function
  | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, lhs), (S.ASSIGN | S.ASSIGN_REF), _)) ->
    Option.to_list (write_event outputs lhs)
  | S.StmFuncCall (_, f, params) ->
    let outs =
      List.filter_map params ~f:(fun (p : S.func_param_assign) ->
          match p.stmt with
          | S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) -> write_event outputs v
          | _ -> None)
    in
    let inst = S.Function.get_name f in
    (* A function block writes outputs when it is called. *)
    let body =
      Option.value_map (Map.find instances inst) ~default:[] ~f:(fun ty ->
          call_writes ~effects ~globals ~ti:(S.Function.get_ti f) inst ty)
    in
    body @ outs
  | _ -> []

let check_elem ~effects globals elem =
  let decls = AU.get_var_decls elem in
  let outputs = visible_outputs globals decls in
  let events = events ~effects ~globals ~instances:(instance_types decls) outputs in
  Once_per_cycle.find_repeated ~events (AU.get_top_stmts elem)
  |> List.map ~f:(fun (r : Once_per_cycle.repeat) ->
      let where =
        if r.in_loop then "it is written in a loop"
        else Printf.sprintf "it is already written on line %d" r.first.linenr
      in
      let msg =
        Printf.sprintf "Physical output %s is written more than once per PLC cycle: %s"
          r.event.shown where
      in
      Warn.mk_at r.event.ti "PLCOPEN-CP12" msg)

(** Outputs written by programs that run in the same task, which are written
    more than once in each cycle of the task. *)
let same_task_writes elements =
  PM.same_task elements ~select:(fun e -> e.PM.writes)
    ~keep:(fun r -> Option.exists r.PM.address ~f:(String.is_prefix ~prefix:"%Q"))
  |> List.map ~f:(fun (c : PM.conflict) ->
      let addr = Option.value c.resolved.address ~default:c.resolved.key in
      let shown =
        match c.resolved.global with
        | Some n -> Printf.sprintf "%s (%s)" n addr
        | None -> addr
      in
      let msg =
        Printf.sprintf
          "Physical output %s is written more than once per PLC cycle: \
           program %s in the same task (%s) also writes it"
          shown c.other.name c.instance.task
      in
      Warn.mk_at c.access.ti "PLCOPEN-CP12" msg)

let do_check elements =
  let globals = global_outputs elements in
  let effects = PM.effects elements in
  let in_pous =
    List.concat_map elements ~f:(function
        | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e ->
          check_elem ~effects globals e
        | _ -> [])
  in
  let at (w : Warn.t) = (w.linenr, w.column) in
  let positions = List.map in_pous ~f:at in
  in_pous
  @ List.filter (same_task_writes elements) ~f:(fun w ->
      not (List.mem positions (at w) ~equal:Poly.equal))

let detector : Detector.t = {
  id = "PLCOPEN-CP12";
  name = "Physical outputs shall be written once per PLC cycle";
  summary =
    "Writing the same output twice in one cycle makes its value depend on \
     statement order.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP12";
  severity = IECCheckerCore.Warn.High;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
