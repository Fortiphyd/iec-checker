open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

(* Global names are those of global variables, declared in configurations,
   resources or VAR_GLOBAL blocks, and of tasks. Local variables with the name
   of a POU or a type are reported by PLCOPEN-N9. *)

let attr_is ~f vd = Option.exists (S.VarDecl.get_attr vd) ~f

let is_global = attr_is ~f:(function S.VarDecl.VarGlobal _ -> true | _ -> false)

(* VAR_EXTERNAL refers to the global rather than shadowing it. *)
let is_external = attr_is ~f:(function S.VarDecl.VarExternal _ -> true | _ -> false)

let collect_globals elems =
  List.concat_map elems ~f:(function
      | S.IECConfiguration (_, c) ->
        let vars =
          c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
        in
        List.map vars ~f:(fun vd -> (S.VarDecl.get_var_name vd, "a global variable"))
        @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
            List.map r.tasks ~f:(fun t -> (S.Task.get_name t, "a task")))
      | elem ->
        AU.get_var_decls elem
        |> List.filter_map ~f:(fun vd ->
            if is_global vd then Some (S.VarDecl.get_var_name vd, "a global variable") else None))
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

let check_elem globals = function
  | S.IECConfiguration _ -> []
  | elem ->
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun vd ->
        if is_global vd || is_external vd then None
        else
          let name = S.VarDecl.get_var_name vd in
          Option.map (Map.find globals name) ~f:(fun what ->
              let msg = Printf.sprintf "Local name %s shadows %s" name what in
              Warn.mk_at (S.VarDecl.get_var_ti vd) "PLCOPEN-N5" msg))

let do_check elems =
  let globals = collect_globals elems in
  if Map.is_empty globals then []
  else List.concat_map elems ~f:(check_elem globals)

let detector : Detector.t = {
  id = "PLCOPEN-N5";
  name = "Local names shall not shadow global names";
  summary =
    "Local variable declarations must not reuse a name already declared at \
     global scope.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N5";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
