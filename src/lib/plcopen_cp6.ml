open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util


(* The rule's exception: referencing VAR_GLOBAL CONSTANT, either declared
   VAR_EXTERNAL CONSTANT or as a plain VAR_EXTERNAL. *)
let constant_globals elems =
  List.concat_map elems ~f:(function
      | S.IECConfiguration (_, c) ->
        c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
      | e -> AU.get_var_decls e)
  |> List.filter_map ~f:(fun d ->
      match S.VarDecl.get_attr d with
      | Some (S.VarDecl.VarGlobal (Some S.VarDecl.QConstant)) -> Some (S.VarDecl.get_var_name d)
      | _ -> None)
  |> String.Set.of_list

let check_elem constants elem =
  match elem with
  | S.IECFunction _ | S.IECFunctionBlock _ | S.IECClass _ ->
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun var_decl ->
        match S.VarDecl.get_attr var_decl with
        | Some (S.VarDecl.VarExternal (Some S.VarDecl.QConstant)) -> None
        | Some (S.VarDecl.VarExternal _)
          when not (Set.mem constants (S.VarDecl.get_var_name var_decl)) ->
          let msg = "External variables in functions, function blocks and classes should be avoided" in
          Some (Warn.mk_at (S.VarDecl.get_var_ti var_decl) "PLCOPEN-CP6" msg)
        | _ -> None)
  | S.IECProgram _ | S.IECConfiguration _ | S.IECType _ | S.IECInterface _ -> []

let do_check elems =
  let constants = constant_globals elems in
  List.concat_map elems ~f:(check_elem constants)

let detector : Detector.t = {
  id = "PLCOPEN-CP6";
  name = "Avoid external variables in functions, function blocks and classes";
  summary =
    "Functions, function blocks and classes should not depend on global state \
     via [VAR_EXTERNAL].";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP6";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
