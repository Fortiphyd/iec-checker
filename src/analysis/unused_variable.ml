open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

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
  and use_set = StringSet.of_list (used @ get_use_var_names ()) in

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
