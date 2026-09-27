open Core
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Report variables whose names don't start with the configured prefixes: a
   scope prefix (GLOBAL, LOCAL, INPUT, ...) followed by a type prefix, e.g.
   [gxReady] for a global BOOL. Either can be configured alone.

   Type prefixes are looked up by the name of a user-defined type, then by
   its kind (STRUCT, ENUM, ARRAY, FUNCTION_BLOCK, ...), then UDT for any
   user-defined type. An alias of an elementary type falls back to the
   elementary type. A prefix must be followed by the start of a word, so
   [xylophone] doesn't have the prefix [x]. *)

let std_fbs = ["TON"; "TOF"; "TP"; "CTU"; "CTD"; "CTUD"; "R_TRIG"; "F_TRIG"; "SR"; "RS"]

let elementary ty = Config.canonical_type_key (S.ety_to_string ty)

(** Keys to look a type prefix up by, most specific first. *)
let type_keys types fbs spec =
  let named n =
    if Set.mem fbs n || List.mem std_fbs n ~equal:String.equal then ["FUNCTION_BLOCK"]
    else
      match Map.find types n with
      | Some (S.DTyDeclSingleElement (S.DTySpecElementary ty, _)) -> [n; "ALIAS"; "UDT"; elementary ty]
      | Some spec -> n :: Naming.type_kinds spec @ ["UDT"]
      | None -> [n]
  in
  match spec with
  | S.DTyDeclSingleElement (S.DTySpecElementary ty, _) -> [elementary ty]
  | S.DTyDeclSingleElement ((S.DTySpecSimple n | S.DTySpecEnum n), _) -> named n
  | S.DTyDeclSingleElement (S.DTySpecGeneric _, _) -> []
  | spec -> Naming.type_kinds spec

(** Keys to look a scope prefix up by. *)
let scope_keys d =
  match S.VarDecl.get_attr d with
  | Some (S.VarDecl.VarGlobal _ | S.VarDecl.VarExternal _) -> ["GLOBAL"]
  | Some (S.VarDecl.Var _) | None -> ["LOCAL"]
  | Some S.VarDecl.VarTemp -> ["TEMP"; "LOCAL"]
  | Some (S.VarDecl.VarIn _) -> ["INPUT"; "PARAMETER"]
  | Some (S.VarDecl.VarOut _) -> ["OUTPUT"; "PARAMETER"]
  | Some S.VarDecl.VarInOut -> ["IN_OUT"; "PARAMETER"]
  | Some _ -> []

let find prefixes keys =
  List.find_map keys ~f:(fun k -> List.Assoc.find prefixes k ~equal:String.equal)

let check_decl cfg types fbs d =
  let type_prefix =
    Option.bind (S.VarDecl.get_ty_spec d) ~f:(fun spec ->
        let keys = type_keys types fbs spec in
        Option.map (find cfg.Config.naming_type_prefixes keys) ~f:(fun p -> (p, List.hd_exn keys)))
  in
  let scope_prefix =
    let keys = scope_keys d in
    Option.map (find cfg.Config.naming_scope_prefixes keys) ~f:(fun p -> (p, List.hd_exn keys))
  in
  match scope_prefix, type_prefix with
  | None, None -> None
  | _ ->
    let prefix =
      Option.value_map scope_prefix ~default:"" ~f:fst
      ^ Option.value_map type_prefix ~default:"" ~f:fst
    in
    let ti = S.VarDecl.get_var_ti d in
    let name = Naming.shown ti (S.VarDecl.get_var_name d) in
    if Naming.has_prefix ~prefix name then None
    else begin
      let what =
        List.filter_opt [
          Option.map scope_prefix ~f:(fun (_, k) -> String.lowercase k);
          Option.map type_prefix ~f:(fun (_, k) -> "of type " ^ k);
        ]
      in
      let msg =
        Printf.sprintf "Variable %s (%s) should start with prefix %S" name
          (String.concat ~sep:", " what) prefix
      in
      Some (Warn.mk_at ti "PLCOPEN-N2" msg)
    end

let do_check elems =
  let cfg = Config.get () in
  if List.is_empty cfg.naming_type_prefixes && List.is_empty cfg.naming_scope_prefixes then []
  else begin
    let types =
      List.filter_map elems ~f:(function
          | S.IECType (_, _, (name, spec)) -> Some (name, spec)
          | _ -> None)
      |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
    in
    let fbs =
      List.filter_map elems ~f:(function
          | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id)
          | _ -> None)
      |> String.Set.of_list
    in
    List.concat_map elems ~f:(fun elem ->
        Naming.var_decls elem |> List.filter_map ~f:(check_decl cfg types fbs))
    |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
        Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))
  end

let detector : Detector.t = {
  id = "PLCOPEN-N2";
  name = "Define type prefixes for variables";
  summary =
    "Variable names should start with configurable scope and type prefixes \
     (Hungarian notation).";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N2";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.Low;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
