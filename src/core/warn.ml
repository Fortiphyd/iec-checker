type warn_ty =
  | Inspection
  | InternalError
[@@deriving yojson]

type severity = Low | Medium | High

let severity_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"

let severity_of_string = function
  | "low" -> Some Low
  | "medium" -> Some Medium
  | "high" -> Some High
  | _ -> None

let severity_rank = function Low -> 0 | Medium -> 1 | High -> 2

let severity_to_yojson s = `String (severity_to_string s)

let severity_of_yojson = function
  | `String s -> begin
      match severity_of_string s with
      | Some s -> Ok s
      | None -> Error ("unknown severity: " ^ s)
    end
  | _ -> Error "severity must be a string"

type t = {
  linenr: int;
  column: int;
  file: string;
  id: string;
  msg: string;
  start_column: int;
  context: string;
  ty: warn_ty [@key "type"];
  severity: severity;
} [@@deriving yojson]

let mk ?(ty=Inspection) ?(file="") ?(context="") ?(severity=Medium) ?start_column linenr column id msg =
  let start_column = Option.value start_column ~default:column in
  { linenr; column; start_column; file; id; msg; context; ty; severity }

let mk_for_name ?severity ~name linenr column id msg =
  let start_column =
    if column > 0 then Int.max 1 (column - String.length name + 1) else column
  in
  mk ?severity ~start_column linenr column id msg

let mk_at ?ty ?file ?context ?severity (ti : Tok_info.t) id msg =
  mk ?ty ?file ?context ?severity ~start_column:ti.start_col ti.linenr ti.col id msg
let mk_internal ?(id="InternalError") msg = mk ~ty:InternalError 0 0 id msg
let mk_from_lexbuf ?(context="") (lexbuf : Lexing.lexbuf) id msg =
  let pos = lexbuf.lex_curr_p and start = lexbuf.lex_start_p in
  let start_column =
    if start.pos_lnum = pos.pos_lnum then start.pos_cnum - start.pos_bol + 1
    else pos.pos_cnum - pos.pos_bol
  in
  mk ~file:(pos.pos_fname) ~context ~start_column pos.pos_lnum (pos.pos_cnum - pos.pos_bol) id msg

let to_string w =
  match w.ty with
  | Inspection -> Printf.sprintf "%d:%d %s: %s" w.linenr w.column w.id w.msg
  | InternalError -> Printf.sprintf "%s: %s" w.id w.msg

