open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn

(* Report outputs, memory and global variables written by program instances
   that run in different tasks. Such tasks can preempt each other, and the
   value left depends on which one ran last.

   Program instances and their tasks come from the RESOURCE blocks of each
   CONFIGURATION. Writes made by function blocks and functions a program
   calls count as writes of the program. Each configuration is a separate
   PLC, so only instances of one configuration are compared. *)

type target =
  | Global of string * bool (** Name, and whether it was declared VAR_EXTERNAL *)
  | Address of string (** Directly represented variable *)

type write = { target : target; ti : TI.t }

(* {{{ Writes of a POU *)
(* Physical outputs and memory. Inputs aren't written by programs. *)
let is_written_address dv =
  match S.DirVar.get_loc dv with
  | Some (S.DirVar.LocQ | S.DirVar.LocM) -> not (S.DirVar.get_is_partly_located dv)
  | Some S.DirVar.LocI | None -> false

let is_external d =
  match S.VarDecl.get_attr d with Some (S.VarDecl.VarExternal _) -> true | _ -> false

let direct_writes elem =
  let decls = AU.get_var_decls elem in
  let externals =
    List.filter decls ~f:is_external |> List.map ~f:S.VarDecl.get_var_name |> String.Set.of_list
  in
  let locals =
    List.filter decls ~f:(Fn.non is_external)
    |> List.map ~f:(fun d -> (S.VarDecl.get_var_name d, S.VarDecl.get_located_at d))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let target_of v =
    match S.VarUse.get_loc v with
    | S.VarUse.DirVar dv ->
      Option.some_if (is_written_address dv) (Address (S.DirVar.get_name dv))
    | S.VarUse.SymVar _ ->
      let name = S.VarUse.get_name v in
      let base = Option.value_map (String.lsplit2 name ~on:'.') ~default:name ~f:fst in
      match Map.find locals base with
      | Some (Some dv) when is_written_address dv -> Some (Address (S.DirVar.get_name dv))
      | Some _ -> None
      (* Declared VAR_EXTERNAL, or used without a declaration, as some
         dialects allow for globals. *)
      | None -> Some (Global (base, Set.mem externals base))
  in
  AU.get_pou_exprs elem
  |> List.filter_map ~f:(function
      | S.ExprBin (_, S.ExprVariable (_, v), (S.ASSIGN | S.ASSIGN_REF), _)
      | S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v)) ->
        Option.map (target_of v) ~f:(fun target -> { target; ti = S.VarUse.get_ti v })
      | _ -> None)

(** Types of the function blocks and names of the functions [elem] calls. *)
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

let pou_name = function
  | S.IECProgram (_, p) -> Some p.name
  | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id)
  | S.IECFunction (_, f) -> Some (S.Function.get_name f.id)
  | _ -> None

(** Writes of each POU, including those of the POUs it calls. *)
let all_writes elements =
  let pous =
    List.filter_map elements ~f:(fun e -> Option.map (pou_name e) ~f:(fun n -> (n, e)))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let memo = String.Table.create () in
  let rec writes visiting name =
    match Hashtbl.find memo name, Map.find pous name with
    | Some ws, _ -> ws
    | None, None -> []
    | None, Some _ when Set.mem visiting name -> [] (* recursion *)
    | None, Some elem ->
      let visiting = Set.add visiting name in
      let ws =
        direct_writes elem
        @ List.concat_map (callees elem) ~f:(writes visiting)
      in
      Hashtbl.set memo ~key:name ~data:ws;
      ws
  in
  writes String.Set.empty
(* }}} *)

(* {{{ Program instances *)
type instance = { name : string; task : string; writes : write list }

type resolved = { key : string; shown : string }

(** Located global variables by name, and the names of all globals. *)
let globals_of decls =
  let located =
    List.filter_map decls ~f:(fun d ->
        Option.map (S.VarDecl.get_located_at d) ~f:(fun dv -> (S.VarDecl.get_var_name d, dv)))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  (located, String.Set.of_list (List.map decls ~f:S.VarDecl.get_var_name))

let resolve ~config_globals ~(resource : S.resource_decl) target =
  let address a =
    let what = if String.is_prefix a ~prefix:"%Q" then "Physical output" else "Memory" in
    { key = a; shown = Printf.sprintf "%s %s" what a }
  in
  match target with
  | Address a -> Some (address a)
  | Global (n, is_ext) ->
    let res_located, res_names = globals_of resource.variables in
    let cfg_located, cfg_names = config_globals in
    match Map.find res_located n, Map.find cfg_located n with
    | Some dv, _ | None, Some dv -> Some (address (S.DirVar.get_name dv))
    | None, None ->
      let global = Some { key = n; shown = Printf.sprintf "Global variable %s" n } in
      (* A resource's globals are its own. *)
      if Set.mem res_names n
      then Some { key = Printf.sprintf "%s.%s" (Option.value resource.name ~default:"") n;
                  shown = Printf.sprintf "Global variable %s" n }
      else if Set.mem cfg_names n then global
      (* Declared in another file. An undeclared name that isn't a global is
         something else, such as the result of a function. *)
      else if is_ext then global
      else None

let instances writes (c : S.configuration_decl) =
  List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
      List.map r.programs ~f:(fun pc ->
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
          let ws = Option.value_map (S.ProgramConfig.get_type_name pc) ~default:[] ~f:writes in
          (r, { name = S.ProgramConfig.get_name pc; task; writes = ws })))
(* }}} *)

let check_configuration writes (c : S.configuration_decl) =
  let config_globals = globals_of c.variables in
  let by_key = String.Table.create () in
  List.iter (instances writes c) ~f:(fun (resource, inst) ->
      List.iter inst.writes ~f:(fun w ->
          Option.iter (resolve ~config_globals ~resource w.target) ~f:(fun r ->
              Hashtbl.add_multi by_key ~key:r.key ~data:(r, inst, w.ti))));
  Hashtbl.data by_key
  |> List.concat_map ~f:(fun entries ->
      let entries = List.rev entries in
      let tasks =
        List.map entries ~f:(fun (_, inst, _) -> inst.task)
        |> List.dedup_and_sort ~compare:String.compare
      in
      if List.length tasks < 2 then []
      else begin
        let writers =
          List.map entries ~f:(fun (_, inst, _) ->
              Printf.sprintf "%s (task %s)" inst.name inst.task)
          |> List.dedup_and_sort ~compare:String.compare
        in
        let r, _, _ = List.hd_exn entries in
        let msg =
          Printf.sprintf "%s is written by programs in different tasks: %s"
            r.shown (String.concat ~sep:", " writers)
        in
        List.map entries ~f:(fun (_, _, (ti : TI.t)) -> (ti, msg))
      end)

let run elements =
  let writes = all_writes elements in
  let seen = String.Hash_set.create () in
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) -> check_configuration writes c
      | _ -> [])
  (* A write site reached from several instances is reported once. *)
  |> List.filter ~f:(fun ((ti : TI.t), _) ->
      let key = Printf.sprintf "%d:%d" ti.linenr ti.col in
      if Hash_set.mem seen key then false else (Hash_set.add seen key; true))
  |> List.sort ~compare:(fun ((a : TI.t), _) ((b : TI.t), _) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.col) (b.linenr, b.col))
  |> List.map ~f:(fun ((ti : TI.t), msg) -> Warn.mk ti.linenr ti.col "MultiTaskWrite" msg)
