open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

let shown (ti : Tok_info.t) name = if String.is_empty ti.raw then name else ti.raw

let has_prefix ~prefix name =
  String.is_prefix name ~prefix
  && (String.is_suffix prefix ~suffix:"_"
      || String.length name = String.length prefix
      || (let c = name.[String.length prefix] in
          Char.is_uppercase c || Char.is_digit c || Char.equal c '_'))

let type_kinds = function
  | S.DTyDeclEnumType (Some _, _, _) -> ["NAMED"; "ENUM"]
  | spec -> [S.dty_decl_spec_kind_to_string spec]

let element_kinds = function
  | S.IECType (_, _, (_, spec)) -> type_kinds spec @ ["UDT"]
  | S.IECFunction _ -> ["FUNCTION"]
  | S.IECFunctionBlock _ -> ["FUNCTION_BLOCK"]
  | S.IECProgram _ -> ["PROGRAM"]
  | S.IECClass _ -> ["CLASS"]
  | S.IECInterface _ -> ["INTERFACE"]
  | S.IECConfiguration _ -> []

let strip_prefix prefixes keys name =
  match List.find_map keys ~f:(fun k -> List.Assoc.find prefixes k ~equal:String.equal) with
  | Some prefix when has_prefix ~prefix name ->
    let rest = String.drop_prefix name (String.length prefix) in
    Option.value (String.chop_prefix rest ~prefix:"_") ~default:rest
  | _ -> name

let var_decls = function
  | S.IECConfiguration (_, c) ->
    c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
  | e -> AU.get_var_decls e
