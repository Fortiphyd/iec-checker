open Core
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

(** Keywords / reserved word list of IEC 61131-3 Ed.3 starting with a letter *)
let reserved_keywords =
  String.Set.of_list [
    "ABS"; "ABSTRACT"; "ACOS"; "ACTION"; "ADD"; "AND"; "ARRAY"; "ASIN"; "AT";
    "ATAN"; "ATAN2"; "BOOL"; "BY"; "BYTE"; "CASE"; "CHAR"; "CLASS"; "CONCAT";
    "CONFIGURATION"; "CONSTANT"; "CONTINUE"; "COS"; "CTD"; "CTU"; "CTUD"; "DATE";
    "DATE_AND_TIME"; "DELETE"; "DINT"; "DIV"; "DO"; "DT"; "DWORD"; "ELSE"; "ELSIF";
    "END_ACTION"; "END_CASE"; "END_CLASS"; "END_CONFIGURATION"; "END_FOR"; "END_FUNCTION";
    "END_FUNCTION_BLOCK"; "END_IF"; "END_INTERFACE"; "END_METHOD"; "END_NAMESPACE";
    "END_PROGRAM"; "END_REPEAT"; "END_RESOURCE"; "END_STEP"; "END_STRUCT"; "END_TRANSITION";
    "END_TYPE"; "END_VAR"; "END_WHILE"; "EQ"; "EXIT"; "EXP"; "EXPT"; "EXTENDS";
    "F_EDGE"; "F_TRIG"; "FALSE"; "FINAL"; "FIND"; "FOR"; "FROM"; "FUNCTION";
    "FUNCTION_BLOCK"; "GE"; "GT"; "IF"; "IMPLEMENTS"; "INITIAL_STEP"; "INSERT";
    "INT"; "INTERFACE"; "INTERNAL"; "INTERVAL"; "LD"; "LDATE"; "LDATE_AND_TIME";
    "LDT"; "LE"; "LEFT"; "LEN"; "LIMIT"; "LINT"; "LN"; "LOG"; "LREAL"; "LT";
    "LTIME"; "LTIME_OF_DAY"; "LTOD"; "LWORD"; "MAX"; "METHOD"; "MID"; "MIN";
    "MOD"; "MOVE"; "MUL"; "MUX"; "NAMESPACE"; "NE"; "NON_RETAIN"; "NOT"; "NULL";
    "OF"; "ON"; "OR"; "OVERLAP"; "OVERRIDE"; "PRIORITY"; "PRIVATE"; "PROGRAM";
    "PROTECTED"; "PUBLIC"; "R_EDGE"; "R_TRIG"; "READ_ONLY"; "READ_WRITE"; "REAL";
    "REF"; "REF_TO"; "REPEAT"; "REPLACE"; "RESOURCE"; "RETAIN"; "RETURN"; "RIGHT";
    "ROL"; "ROR"; "RS"; "SEL"; "SHL"; "SHR"; "SIN"; "SINGLE"; "SINT"; "SQRT";
    "SR"; "STEP"; "STRING"; "STRUCT"; "SUB"; "SUPER"; "T"; "TAN"; "TASK"; "THEN";
    "THIS"; "TIME"; "TIME_OF_DAY"; "TO"; "TOD"; "TOF"; "TON"; "TP"; "TRANSITION";
    "TRUE"; "TRUNC"; "TYPE"; "UDINT"; "UINT"; "ULINT"; "UNTIL"; "USING"; "USINT";
    "VAR"; "VAR_ACCESS"; "VAR_CONFIG"; "VAR_EXTERNAL"; "VAR_GLOBAL"; "VAR_IN_OUT";
    "VAR_INPUT"; "VAR_OUTPUT"; "VAR_TEMP"; "WCHAR"; "WHILE"; "WITH"; "WORD";
    "WSTRING"; "XOR";
  ]

let reserved name = Set.mem reserved_keywords (String.uppercase name)

let warn (ti : TI.t) what name =
  Warn.mk_at ti "PLCOPEN-N3"
    (Printf.sprintf "%s %s is a reserved word of IEC 61131-3 and should be avoided" what name)

(** Names of the members of a type: enum values and struct elements. *)
let member_names = function
  | S.DTyDeclEnumType (_, elems, _) ->
    List.map elems ~f:(fun (e : S.enum_element_spec) -> ("Enum value", e.elem_name))
  | S.DTyDeclStructType (_, elems) ->
    List.map elems ~f:(fun (e : S.struct_elem_spec) -> ("Struct member", e.struct_elem_name))
  | _ -> []

let check_elem elem =
  let vars =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun d ->
        let name = S.VarDecl.get_var_name d in
        Option.some_if (reserved name) (warn (S.VarDecl.get_var_ti d) "Variable" name))
  in
  let own =
    match elem, S.get_pou_name_as_written elem with
    | S.IECConfiguration _, _ | _, None -> []
    | _, Some (name, ti) ->
      let what = match elem with S.IECType _ -> "Type" | _ -> "POU" in
      (if reserved name then [warn ti what name] else [])
      @ (match elem with
          | S.IECType (_, _, (_, spec)) ->
            (* Members have no position of their own; report them at the type. *)
            List.filter_map (member_names spec) ~f:(fun (what, member) ->
                Option.some_if (reserved member) (warn ti what member))
          | _ -> [])
  in
  own @ vars

let do_check elems = List.concat_map elems ~f:check_elem

let detector : Detector.t = {
  id = "PLCOPEN-N3";
  name = "Define the names to avoid";
  summary =
    "Variable names must not collide with IEC 61131-3 keywords or standard \
     library identifiers.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N3";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
