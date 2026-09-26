open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info

type target =
  | Global of string * bool
  | Address of string

type access = { target : target; ti : TI.t; pou : string }

type effects = { writes : access list; calls : access list }

(* {{{ Effects of a POU *)
(* Physical outputs and memory. Inputs aren't written by programs. *)
let is_written_address dv =
  match S.DirVar.get_loc dv with
  | Some (S.DirVar.LocQ | S.DirVar.LocM) -> not (S.DirVar.get_is_partly_located dv)
  | Some S.DirVar.LocI | None -> false

let is_external d =
  match S.VarDecl.get_attr d with Some (S.VarDecl.VarExternal _) -> true | _ -> false

let pou_name = function
  | S.IECProgram (_, p) -> Some p.name
  | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id)
  | S.IECFunction (_, f) -> Some (S.Function.get_name f.id)
  | _ -> None

let direct_effects pou_names elem =
  let pou = Option.value (pou_name elem) ~default:"" in
  let decls = AU.get_var_decls elem in
  let externals =
    List.filter decls ~f:is_external |> List.map ~f:S.VarDecl.get_var_name |> String.Set.of_list
  in
  let locals =
    List.filter decls ~f:(Fn.non is_external)
    |> List.map ~f:(fun d -> (S.VarDecl.get_var_name d, S.VarDecl.get_located_at d))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  (* Declared VAR_EXTERNAL, or used without a declaration, as some dialects
     allow for globals. *)
  let global name = Global (name, Set.mem externals name) in
  let write_target v =
    match S.VarUse.get_loc v with
    | S.VarUse.DirVar dv ->
      Option.some_if (is_written_address dv) (Address (S.DirVar.get_name dv))
    | S.VarUse.SymVar _ ->
      let name = S.VarUse.get_name v in
      let base = Option.value_map (String.lsplit2 name ~on:'.') ~default:name ~f:fst in
      match Map.find locals base with
      | Some (Some dv) when is_written_address dv -> Some (Address (S.DirVar.get_name dv))
      | Some _ -> None
      | None -> Some (global base)
  in
  let writes =
    AU.get_pou_exprs elem
    |> List.filter_map ~f:(function
        | S.ExprBin (_, S.ExprVariable (_, v), (S.ASSIGN | S.ASSIGN_REF), _)
        | S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v)) ->
          Option.map (write_target v) ~f:(fun target -> { target; ti = S.VarUse.get_ti v; pou })
        | _ -> None)
  in
  (* Calls of function block instances that aren't declared in the POU, i.e.
     globals. Calls of functions are by the name of their declaration. *)
  let calls =
    AU.get_pou_stmts elem
    |> List.filter_map ~f:(function
        | S.StmFuncCall (_, f, _) ->
          let name = S.Function.get_name f in
          if Map.mem locals name || Set.mem pou_names name then None
          else Some { target = global name; ti = S.Function.get_ti f; pou }
        | _ -> None)
  in
  { writes; calls }

let callees elem =
  let instances =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  AU.get_pou_stmts elem
  |> List.filter_map ~f:(function
      | S.StmFuncCall (_, f, _) ->
        let name = S.Function.get_name f in
        Some (Option.value (Map.find instances name) ~default:name)
      | _ -> None)
  |> List.dedup_and_sort ~compare:String.compare

let effects elements =
  let pous =
    List.filter_map elements ~f:(fun e -> Option.map (pou_name e) ~f:(fun n -> (n, e)))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let pou_names = String.Set.of_list (Map.keys pous) in
  let memo = String.Table.create () in
  let none = { writes = []; calls = [] } in
  let rec of_pou visiting name =
    match Hashtbl.find memo name, Map.find pous name with
    | Some e, _ -> e
    | None, None -> none
    | None, Some _ when Set.mem visiting name -> none (* recursion *)
    | None, Some elem ->
      let visiting = Set.add visiting name in
      let own = direct_effects pou_names elem in
      let e =
        List.fold (callees elem) ~init:own ~f:(fun acc callee ->
            let c = of_pou visiting callee in
            { writes = acc.writes @ c.writes; calls = acc.calls @ c.calls })
      in
      Hashtbl.set memo ~key:name ~data:e;
      e
  in
  of_pou String.Set.empty
(* }}} *)

(* {{{ Program instances *)
type instance = {
  name : string;
  type_name : string option;
  task : string;
  resource : S.resource_decl;
}

let instances (c : S.configuration_decl) =
  (* In declaration order; the parser keeps them in reverse. *)
  let by_position pcs =
    List.sort pcs ~compare:(fun a b ->
        let ta = S.ProgramConfig.get_ti a and tb = S.ProgramConfig.get_ti b in
        Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (ta.linenr, ta.col) (tb.linenr, tb.col))
  in
  List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
      List.map (by_position r.programs) ~f:(fun pc ->
          let task =
            match S.ProgramConfig.get_task pc with
            | Some t -> S.Task.get_name t
            | None -> "(none)"
          in
          (* Tasks of different resources run on different CPUs. *)
          let task =
            match r.name with
            | Some res -> Printf.sprintf "%s.%s" res task
            | None -> task
          in
          { name = S.ProgramConfig.get_name pc; type_name = S.ProgramConfig.get_type_name pc;
            task; resource = r }))

type resolved = { key : string; global : string option; address : string option }

(** Located global variables by name, and the names of all globals. *)
let globals_of decls =
  let located =
    List.filter_map decls ~f:(fun d ->
        Option.map (S.VarDecl.get_located_at d) ~f:(fun dv -> (S.VarDecl.get_var_name d, dv)))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  (located, String.Set.of_list (List.map decls ~f:S.VarDecl.get_var_name))

let resolve (c : S.configuration_decl) (resource : S.resource_decl) target =
  let at_address ?global a = Some { key = a; global; address = Some a } in
  match target with
  | Address a -> at_address a
  | Global (n, is_ext) ->
    let res_located, res_names = globals_of resource.variables in
    let cfg_located, cfg_names = globals_of c.variables in
    match Map.find res_located n, Map.find cfg_located n with
    | Some dv, _ | None, Some dv -> at_address ~global:n (S.DirVar.get_name dv)
    | None, None ->
      let global key = Some { key; global = Some n; address = None } in
      (* A resource's globals are its own. *)
      if Set.mem res_names n
      then global (Printf.sprintf "%s.%s" (Option.value resource.name ~default:"") n)
      else if Set.mem cfg_names n then global n
      (* Declared in another file. An undeclared name that isn't a global is
         something else, such as the result of a function. *)
      else if is_ext then global n
      else None
(* }}} *)

(* {{{ Conflicts within a task *)
type conflict = {
  resolved : resolved;
  access : access;
  instance : instance;
  other : instance; (** The first instance of the task with such an access *)
}

let same_task elements ~select ~keep =
  let effects = effects elements in
  let seen = String.Hash_set.create () in
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) ->
        let groups = String.Table.create () in
        List.iter (instances c) ~f:(fun inst ->
            let accesses = Option.value_map inst.type_name ~default:[] ~f:(fun t -> select (effects t)) in
            List.iter accesses ~f:(fun (a : access) ->
                Option.iter (resolve c inst.resource a.target) ~f:(fun r ->
                    if keep r then
                      Hashtbl.add_multi groups ~key:(inst.task ^ "\x00" ^ r.key) ~data:(r, a, inst))));
        Hashtbl.data groups
        |> List.concat_map ~f:(fun entries ->
            match List.rev entries with
            | [] -> []
            | (_, _, first) :: _ as entries ->
              List.filter_map entries ~f:(fun (r, (a : access), inst) ->
                  let key = Printf.sprintf "%s:%d:%d" r.key a.ti.linenr a.ti.col in
                  if String.equal inst.name first.name || Hash_set.mem seen key then None
                  else begin
                    Hash_set.add seen key;
                    Some { resolved = r; access = a; instance = inst; other = first }
                  end))
      | _ -> [])
(* }}} *)
