open Core

type t = { id : int; linenr : int; col : int; start_col : int; raw : string }
[@@deriving yojson, show]

let next_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

let create lexbuf =
  let id = next_id () in
  let start = lexbuf.Lexing.lex_start_p and curr = lexbuf.Lexing.lex_curr_p in
  let linenr = curr.pos_lnum in
  let col = curr.pos_cnum - curr.pos_bol in
  (* A token spanning lines starts at the beginning of its last line. *)
  let start_col =
    if start.pos_lnum = curr.pos_lnum then start.pos_cnum - start.pos_bol + 1 else 1
  in
  { id; linenr; col; start_col; raw = "" }

let create_with_raw lexbuf raw =
  let ti = create lexbuf in
  { ti with raw }

let create_dummy () =
  let id = next_id () in
  let linenr = -1 in
  let col = -1 in
  { id; linenr; col; start_col = -1; raw = "" }

let to_string ti =
  Printf.sprintf "%d:%d" ti.linenr ti.col
