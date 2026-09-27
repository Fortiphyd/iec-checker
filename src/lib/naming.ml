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

let var_decls = function
  | S.IECConfiguration (_, c) ->
    c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
  | e -> AU.get_var_decls e
