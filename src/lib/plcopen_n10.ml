open Core
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Report user-defined types and POUs whose names don't start with the prefix
   configured for their kind. Types are looked up by kind (STRUCT, ENUM,
   ...), then UDT for any user-defined type. Prefixes are compared with the
   name as written and must be followed by the start of a word, so [Error]
   doesn't have the prefix [E] but [E_Error] and [EError] do. *)

let check_prefix prefixes keys name (ti : IECCheckerCore.Tok_info.t) =
  match List.find_map keys ~f:(fun k ->
      Option.map (List.Assoc.find prefixes k ~equal:String.equal) ~f:(fun p -> (k, p))) with
  | None -> None
  | Some (_, prefix) when Naming.has_prefix ~prefix name -> None
  | Some (kind, prefix) ->
    let msg = Printf.sprintf "%s %s should start with prefix %S" kind name prefix in
    Some (Warn.mk_for_name ~name ti.linenr ti.col "PLCOPEN-N10" msg)

let check_elem prefixes e =
  Option.bind (S.get_pou_name_as_written e) ~f:(fun (name, ti) ->
      check_prefix prefixes (Naming.element_kinds e) name ti)
  |> Option.to_list

let do_check elems =
  let prefixes = (Config.get ()).naming_udt_prefixes in
  if List.is_empty prefixes then []
  else List.concat_map elems ~f:(check_elem prefixes)

let detector : Detector.t = {
  id = "PLCOPEN-N10";
  name = "Define name prefixes for user defined types";
  summary =
    "User-defined types and POUs should start with a configurable prefix \
     based on their kind.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N10";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.Low;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
