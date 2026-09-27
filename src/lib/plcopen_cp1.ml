open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

(** Addresses of the located variables declared in configurations and
    resources, with the names of the variables. *)
let located_globals elems =
  List.concat_map elems ~f:(function
      | S.IECConfiguration (_, c) ->
        c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
      | _ -> [])

let located_names decls =
  List.filter_map decls ~f:(fun d ->
      Option.map (S.VarDecl.get_located_at d) ~f:(fun dv ->
          (S.DirVar.get_name dv, S.VarDecl.get_var_name d)))

(** Direct accesses, read or written, to an address that has a name. *)
let check_elem globals elem =
  let named =
    String.Map.of_alist_reduce ~f:(fun first _ -> first)
      (located_names (AU.get_var_decls elem) @ globals)
  in
  AU.get_var_uses elem
  |> List.filter_map ~f:(fun v ->
      match S.VarUse.get_loc v with
      | S.VarUse.DirVar dv ->
        let addr = S.DirVar.get_name dv in
        Option.map (Map.find named addr) ~f:(fun name ->
            let msg = Printf.sprintf "Access to a member %s shall be by name (%s)" addr name in
            Warn.mk_at (S.VarUse.get_ti v) "PLCOPEN-CP1" msg)
      | S.VarUse.SymVar _ -> None)

let do_check elems =
  let globals = located_names (located_globals elems) in
  List.concat_map elems ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        check_elem globals e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP1";
  name = "Access to a member shall be by name";
  summary =
    "Direct addressing should not be used when a symbolic name exists.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP1";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
