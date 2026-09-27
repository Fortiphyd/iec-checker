open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

(** Names called in [e]: function block instances are used by calling them. *)
let calls e =
  let rec in_expr = function
    | S.ExprFuncCall (_, S.StmFuncCall (_, f, _)) -> [S.Function.get_name f]
    | S.ExprBin (_, a, _, b) -> in_expr a @ in_expr b
    | S.ExprUn (_, _, a) -> in_expr a
    | S.ExprFuncCall _ | S.ExprVariable _ | S.ExprConstant _ -> []
  in
  List.filter_map (AU.get_pou_stmts e) ~f:(function
      | S.StmFuncCall (_, f, _) -> Some (S.Function.get_name f)
      | _ -> None)
  @ List.concat_map (AU.get_pou_exprs e) ~f:in_expr

let check_pou ?(used = []) elem =
  let module StringSet = Set.Make(String) in

  (* Get names of variables declared in POU. *)
  let get_decl_var_names () =
    AU.get_var_decls elem
    |> List.map ~f:(fun vardecl -> S.VarDecl.get_var_name vardecl)
  in

  (* Get names of variables used in POU. *)
  let base_name name =
    match String.lsplit2 name ~on:'.' with
    | Some (hd, _) -> hd
    | None -> name
  in
  let get_use_var_names () =
    AU.filter_exprs
      elem
      ~f:(fun expr -> begin
            match expr with S.ExprVariable _ -> true | _ -> false
          end)
    |> List.map
      ~f:(fun expr -> begin
            match expr with
            | S.ExprVariable (_, v) -> base_name (S.VarUse.get_name v)
            | _ -> assert false
          end)
  in

  let decl_set = StringSet.of_list (get_decl_var_names ())
  and use_set = StringSet.of_list (used @ calls elem @ get_use_var_names ()) in

  Set.diff decl_set use_set
  |> Set.fold ~init:[]
    ~f:(fun acc var_name -> begin
          let ti = AU.get_ti_by_name_exn elem var_name in
          let text = Printf.sprintf "Found unused local variable: %s" var_name in
          acc @ [Warn.mk_at ti "UnusedVariable" text]
        end)

(** Function block instances of each program type that configurations
    assign to tasks, which run them. *)
let task_instances elements =
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) ->
        List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
            List.filter_map r.programs ~f:(fun pc ->
                Option.map (S.ProgramConfig.get_type_name pc) ~f:(fun tn ->
                    (tn, List.map (S.ProgramConfig.get_fb_tasks pc)
                       ~f:(fun (t : S.ProgramConfig.fb_task) -> t.fb_name)))))
      | _ -> [])
  |> String.Map.of_alist_reduce ~f:( @ )

(** Global variables of configurations that nothing uses: no POU, no
    connection of a program instance and no task input. Only reported when
    the analyzed code declares every program the configuration runs, as the
    globals may be used by code that isn't analyzed otherwise. *)
let unused_globals elements =
  let programs =
    List.filter_map elements ~f:(function S.IECProgram (_, p) -> Some p.name | _ -> None)
    |> String.Set.of_list
  in
  let base v =
    let name = S.VarUse.get_name v in
    Option.value_map (String.lsplit2 name ~on:'.') ~default:name ~f:fst
  in
  let in_pous =
    List.concat_map elements ~f:(function
        | S.IECConfiguration _ -> []
        | e -> List.map (AU.get_var_uses e) ~f:base @ calls e)
  in
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) ->
        let instances = List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.programs) in
        let runs_known =
          (not (List.is_empty instances))
          && List.for_all instances ~f:(fun pc ->
              Option.exists (S.ProgramConfig.get_type_name pc) ~f:(Set.mem programs))
        in
        if not runs_known then []
        else begin
          let in_config =
            List.concat_map instances ~f:(fun pc ->
                List.filter_map (S.ProgramConfig.get_connections pc)
                  ~f:(fun (cn : S.ProgramConfig.connection) -> Option.map cn.other ~f:base))
            @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
                List.concat_map r.tasks ~f:(fun t ->
                    List.filter_map [S.Task.get_interval t; S.Task.get_single t] ~f:(function
                        | Some (S.Task.DSGlobalVar v) -> Some (base v)
                        (* A name alone can parse as an enumerated value. *)
                        | Some (S.Task.DSConstant (S.CEnumValue (_, n))) -> Some n
                        | _ -> None)))
          in
          let used = String.Set.of_list (in_pous @ in_config) in
          (* A located global is used through its address too. *)
          let address d =
            Option.map (S.VarDecl.get_located_at d) ~f:S.DirVar.get_name
          in
          c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
          |> List.filter ~f:(fun d ->
              not (Set.mem used (S.VarDecl.get_var_name d))
              && not (Option.exists (address d) ~f:(Set.mem used)))
          |> List.map ~f:(fun d ->
              let ti = S.VarDecl.get_var_ti d in
              let name = if String.is_empty ti.raw then S.VarDecl.get_var_name d else ti.raw in
              Warn.mk_at ti "UnusedVariable" (Printf.sprintf "Found unused global variable: %s" name))
        end
      | _ -> [])

let run elements =
  let task_instances = task_instances elements in
  List.fold_left
    elements
    ~f:(fun warns e ->
        let ws = match e with
          | S.IECProgram (_, p) ->
            check_pou ~used:(Option.value (Map.find task_instances p.name) ~default:[]) e
          | S.IECFunction _ | S.IECFunctionBlock _ -> check_pou e
          | _ -> []
        in
        warns @ ws)
    ~init:[]
  @ unused_globals elements
