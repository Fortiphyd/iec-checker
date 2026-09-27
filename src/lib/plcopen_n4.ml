open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Check names against the case style configured for their kind: variables,
   constants, POUs, types, struct members (the variable style if not set)
   and enum values (the constant style if not set). The styles are those the
   rule lists; Config rejects others. *)

(* Recognized case style names. *)
let upper_camel = "UpperCamelCase"
let lower_camel = "lowerCamelCase"
let upper_snake = "UPPER_SNAKE_CASE"
let lower_snake = "lower_snake_case"
let all_lower = "alllowercase"
let all_upper = "ALLUPPERCASE"

(* All capitals, like STARTMOTOR, isn't UpperCamelCase: the rule gives it as
   an example not to follow. Two-letter names like IO are. *)
let is_upper_camel s =
  (not (String.is_empty s))
  && Char.is_uppercase s.[0]
  && String.for_all s ~f:Char.is_alphanum
  && (String.length s <= 2 || String.exists s ~f:Char.is_lowercase)

let is_lower_camel s =
  (not (String.is_empty s))
  && Char.is_lowercase s.[0]
  && String.for_all s ~f:Char.is_alphanum

let is_upper_snake s =
  (not (String.is_empty s))
  && not (Char.is_digit s.[0])
  && String.for_all s ~f:(fun c ->
      Char.is_uppercase c || Char.is_digit c || Char.equal c '_')

let is_lower_snake s =
  (not (String.is_empty s))
  && not (Char.is_digit s.[0])
  && String.for_all s ~f:(fun c ->
      Char.is_lowercase c || Char.is_digit c || Char.equal c '_')

let is_all_lower s =
  (not (String.is_empty s))
  && Char.is_lowercase s.[0]
  && String.for_all s ~f:(fun c -> Char.is_lowercase c || Char.is_digit c)

let is_all_upper s =
  (not (String.is_empty s))
  && Char.is_uppercase s.[0]
  && String.for_all s ~f:(fun c -> Char.is_uppercase c || Char.is_digit c)

let predicate_of_style = function
  | "UpperCamelCase" -> Some (is_upper_camel, upper_camel)
  | "lowerCamelCase" -> Some (is_lower_camel, lower_camel)
  | "UPPER_SNAKE_CASE" -> Some (is_upper_snake, upper_snake)
  | "lower_snake_case" -> Some (is_lower_snake, lower_snake)
  | "alllowercase" -> Some (is_all_lower, all_lower)
  | "ALLUPPERCASE" -> Some (is_all_upper, all_upper)
  | _ -> None

let is_constant vd =
  match S.VarDecl.get_attr vd with
  | Some (S.VarDecl.Var (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarOut (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarIn (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarExternal (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarGlobal (Some S.VarDecl.QConstant)) -> true
  | _ -> false

(** [checked] is the part of [name] that has the style, if not all of it. *)
let check_identifier ?checked ~style name linenr col =
  match Option.bind style ~f:predicate_of_style with
  | None -> None
  | Some (pred, label) ->
    let checked = Option.value checked ~default:name in
    if String.is_empty checked || pred checked then None
    else
      let msg = Printf.sprintf
          "Identifier %s does not match required case %s" name label
      in
      Some (Warn.mk_for_name ~name linenr col "PLCOPEN-N4" msg)

let display_name_of_ti ti fallback =
  if String.is_empty ti.IECCheckerCore.Tok_info.raw then fallback else ti.raw

let check_var_decl cfg vd =
  let ti = S.VarDecl.get_var_ti vd in
  let name = display_name_of_ti ti (S.VarDecl.get_var_name vd) in
  let style =
    if is_constant vd then cfg.Config.naming_case_constant
    else cfg.Config.naming_case_variable
  in
  check_identifier ~style name ti.linenr ti.col

let pou_name_and_loc = function
  | S.IECFunction (_, f) ->
    let ti = S.Function.get_ti f.id in
    let name = display_name_of_ti ti (S.Function.get_name f.id) in
    Some (name, ti.linenr, ti.col)
  | S.IECFunctionBlock (_, fb) ->
    let ti = S.FunctionBlock.get_ti fb.id in
    let name = display_name_of_ti ti (S.FunctionBlock.get_name fb.id) in
    Some (name, ti.linenr, ti.col)
  | S.IECProgram _ | S.IECClass _ | S.IECInterface _ as e ->
    Option.map (S.get_pou_name_as_written e) ~f:(fun (name, ti) -> (name, ti.linenr, ti.col))
  | S.IECConfiguration _ | S.IECType _ -> None

let type_name_and_loc = function
  | S.IECType _ as e ->
    Option.map (S.get_pou_name_as_written e) ~f:(fun (name, ti) -> (name, ti.linenr, ti.col))
  | _ -> None

(** Struct members and enum values declared by a type specification. *)
let members_and_values cfg spec =
  let member_style = Option.first_some cfg.Config.naming_case_member cfg.Config.naming_case_variable in
  let value_style = Option.first_some cfg.Config.naming_case_enum_value cfg.Config.naming_case_constant in
  match spec with
  | S.DTyDeclStructType (_, elems) ->
    List.filter_map elems ~f:(fun (e : S.struct_elem_spec) ->
        let ti = e.struct_elem_ti in
        check_identifier ~style:member_style (Naming.shown ti e.struct_elem_name) ti.linenr ti.col)
  | S.DTyDeclEnumType (_, elems, _) ->
    List.filter_map elems ~f:(fun (e : S.enum_element_spec) ->
        let ti = e.elem_ti in
        check_identifier ~style:value_style (Naming.shown ti e.elem_name) ti.linenr ti.col)
  | _ -> []

let check_elem cfg elem =
  let decls = Naming.var_decls elem in
  let var_warns = List.filter_map decls ~f:(check_var_decl cfg) in
  let member_warns =
    (match elem with S.IECType (_, _, (_, spec)) -> [spec] | _ -> [])
    @ List.filter_map decls ~f:S.VarDecl.get_ty_spec
    |> List.concat_map ~f:(members_and_values cfg)
  in
  (* The case applies after the prefix of the name (PLCopen N10), which the
     rule proposes in capitals: PRG_Main is UpperCamelCase with prefix PRG_. *)
  let without_prefix name =
    Naming.strip_prefix cfg.Config.naming_udt_prefixes (Naming.element_kinds elem) name
  in
  let pou_warn =
    match pou_name_and_loc elem with
    | Some (name, linenr, col) ->
      check_identifier ~checked:(without_prefix name) ~style:cfg.Config.naming_case_pou
        name linenr col
    | None -> None
  in
  let type_warn =
    match type_name_and_loc elem with
    | Some (name, linenr, col) ->
      check_identifier ~checked:(without_prefix name) ~style:cfg.Config.naming_case_type
        name linenr col
    | None -> None
  in
  var_warns
  @ member_warns
  @ (Option.to_list pou_warn)
  @ (Option.to_list type_warn)

let do_check elems =
  let cfg = Config.get () in
  let any_case =
    Option.is_some cfg.naming_case_variable
    || Option.is_some cfg.naming_case_constant
    || Option.is_some cfg.naming_case_pou
    || Option.is_some cfg.naming_case_type
    || Option.is_some cfg.naming_case_member
    || Option.is_some cfg.naming_case_enum_value
  in
  if not any_case then []
  else List.concat_map elems ~f:(check_elem cfg)

let detector : Detector.t = {
  id = "PLCOPEN-N4";
  name = "Define the use of case (capitals)";
  summary =
    "Identifiers should follow a configurable naming convention per element \
     kind (variable, constant, POU, type).";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N4";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
