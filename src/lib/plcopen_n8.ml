open Core
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Report names with characters outside ASCII letters, digits and
   underscores, such as accented or national letters, and names with
   consecutive or trailing underscores, which IEC 61131-3 (6.1.2) doesn't
   allow. The rule's exception for a national character set is the
   allow_non_ascii option.

   Names are checked as written: POUs, types, variables, struct members,
   enum values, tasks and program instances. Names starting with a digit
   aren't valid in the language and fail to parse. *)

(** The characters of a UTF-8 encoded string outside ASCII, each once. *)
let non_ascii_chars name =
  let n = String.length name in
  let rec go i acc =
    if i >= n then List.rev acc
    else if Char.to_int name.[i] < 0x80 then go (i + 1) acc
    else begin
      (* A lead byte and the continuation bytes (10xxxxxx) after it *)
      let j = ref (i + 1) in
      while !j < n && Char.to_int name.[!j] land 0xC0 = 0x80 do incr j done;
      let c = String.sub name ~pos:i ~len:(!j - i) in
      go !j (if List.mem acc c ~equal:String.equal then acc else c :: acc)
    end
  in
  go 0 []

let check_name ~allow_non_ascii name (ti : TI.t) =
  let name = Naming.shown ti name in
  let chars = if allow_non_ascii then [] else non_ascii_chars name in
  let other =
    String.to_list name
    |> List.filter ~f:(fun c ->
        Char.to_int c < 0x80 && not (Char.is_alphanum c || Char.equal c '_'))
    |> List.dedup_and_sort ~compare:Char.compare
    |> List.map ~f:String.of_char
  in
  let problems =
    List.filter_opt [
      Option.some_if (not (List.is_empty (chars @ other)))
        (Printf.sprintf "characters outside ASCII letters, digits and underscores (%s)"
           (String.concat ~sep:" " (chars @ other)));
      Option.some_if (String.is_substring name ~substring:"__") "consecutive underscores";
      Option.some_if (String.length name > 1 && String.is_suffix name ~suffix:"_")
        "a trailing underscore";
    ]
  in
  if List.is_empty problems then None
  else
    let msg = Printf.sprintf "Identifier %s contains %s" name (String.concat ~sep:" and " problems) in
    Some (Warn.mk_for_name ~name ti.linenr ti.col "PLCOPEN-N8" msg)

(** Names declared by an element, with their positions. *)
let names elem =
  let decls =
    List.map (Naming.var_decls elem) ~f:(fun d -> (S.VarDecl.get_var_name d, S.VarDecl.get_var_ti d))
  in
  let members = function
    | S.DTyDeclStructType (_, elems) ->
      List.map elems ~f:(fun (e : S.struct_elem_spec) -> (e.struct_elem_name, e.struct_elem_ti))
    | S.DTyDeclEnumType (_, elems, _) ->
      List.map elems ~f:(fun (e : S.enum_element_spec) -> (e.elem_name, e.elem_ti))
    | _ -> []
  in
  let specs =
    (match elem with S.IECType (_, _, (_, spec)) -> [spec] | _ -> [])
    @ List.filter_map (Naming.var_decls elem) ~f:S.VarDecl.get_ty_spec
  in
  let config =
    match elem with
    | S.IECConfiguration (_, c) ->
      List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
          List.map r.tasks ~f:(fun t -> (S.Task.get_name t, S.Task.get_ti t))
          @ List.map r.programs ~f:(fun pc -> (S.ProgramConfig.get_name pc, S.ProgramConfig.get_ti pc)))
    | _ -> []
  in
  Option.to_list (S.get_pou_name_as_written elem) @ decls @ List.concat_map specs ~f:members @ config

let do_check elems =
  let allow_non_ascii = (Config.get ()).naming_allow_non_ascii in
  List.concat_map elems ~f:(fun e ->
      List.filter_map (names e) ~f:(fun (name, ti) -> check_name ~allow_non_ascii name ti))
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-N8";
  name = "Define the acceptable character set";
  summary =
    "Identifiers should only contain ASCII letters, digits and single \
     underscores.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N8";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
