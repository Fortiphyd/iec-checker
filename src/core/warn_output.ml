open Core
module W = Warn

type output_format =
  | Plain
  | Json
  | Sarif

type rule = {
  rule_id : string;
  rule_name : string;
  help_url : string;
  rule_severity : W.severity;
}

(* ANSI escape helpers *)
let bold s use_color = if use_color then "\027[1m" ^ s ^ "\027[0m" else s
let blue s use_color = if use_color then "\027[34m" ^ s ^ "\027[0m" else s
let cyan s use_color = if use_color then "\027[36m" ^ s ^ "\027[0m" else s

let format_plain_warning doc_urls use_color (w : W.t) =
  match w.ty with
  | W.InternalError ->
    Printf.sprintf "%s: %s" (bold w.id use_color) w.msg
  | W.Inspection ->
    let header =
      Printf.sprintf "%s [%s]: %s" (bold w.id use_color) (W.severity_to_string w.severity) w.msg
    in
    let location =
      if String.is_empty w.file then
        Printf.sprintf "  %s %d:%d" (blue "-->" use_color) w.linenr w.column
      else
        Printf.sprintf "  %s %s:%d:%d" (blue "-->" use_color) w.file w.linenr w.column
    in
    let context_block =
      if String.is_empty w.context then ""
      else "\n" ^ w.context
    in
    let doc_line =
      match List.Assoc.find doc_urls ~equal:String.equal w.id with
      | Some url when not (String.is_empty url) ->
        Printf.sprintf "\n  %s %s" (cyan "See:" use_color) url
      | _ -> ""
    in
    header ^ "\n" ^ location ^ context_block ^ doc_line

(* {{{ SARIF *)
let sarif_level = function
  | W.High -> "error"
  | W.Medium -> "warning"
  | W.Low -> "note"

let sarif_uri path = String.map path ~f:(function '\\' -> '/' | c -> c)

let sarif_rule r =
  `Assoc ([
      "id", `String r.rule_id;
      "shortDescription", `Assoc ["text", `String r.rule_name];
      "defaultConfiguration", `Assoc ["level", `String (sarif_level r.rule_severity)];
    ] @ (if String.is_empty r.help_url then [] else ["helpUri", `String r.help_url]))

let sarif_result (w : W.t) =
  (* Line 0 means the warning has no position. *)
  let region =
    if w.linenr > 0 then
      ["region", `Assoc (["startLine", `Int w.linenr]
                         @ (if w.column > 0 then ["startColumn", `Int w.column] else []))]
    else []
  in
  let locations =
    if String.is_empty w.file then []
    else ["locations", `List [
        `Assoc ["physicalLocation",
                `Assoc (["artifactLocation", `Assoc ["uri", `String (sarif_uri w.file)]] @ region)]]]
  in
  `Assoc ([
      "ruleId", `String w.id;
      "level", `String (sarif_level w.severity);
      "message", `Assoc ["text", `String w.msg];
    ] @ locations)

let sarif_report rules warnings : Yojson.Safe.t =
  `Assoc [
    "$schema", `String "https://json.schemastore.org/sarif-2.1.0.json";
    "version", `String "2.1.0";
    "runs", `List [`Assoc [
        "tool", `Assoc ["driver", `Assoc [
            "name", `String "iec-checker";
            "informationUri", `String "https://github.com/iec-checker/iec-checker";
            "rules", `List (List.map rules ~f:sarif_rule);
          ]];
        "results", `List (List.map warnings ~f:sarif_result);
      ]];
  ]
(* }}} *)

let print_report ?(doc_urls=[]) ?(use_color=true) ?(rules=[]) warnings fmt =
  match fmt with
  | Plain ->
    if not (List.is_empty warnings) then begin
      List.map warnings ~f:(format_plain_warning doc_urls use_color)
      |> String.concat ~sep:"\n\n"
      |> Printf.printf "%s\n"
    end
  | Json ->
    let json_list = List.map warnings ~f:W.to_yojson in
    Yojson.Safe.to_string (`List json_list)
    |> Printf.printf "%s\n"
  | Sarif ->
    Yojson.Safe.pretty_to_string (sarif_report rules warnings)
    |> Printf.printf "%s\n"
