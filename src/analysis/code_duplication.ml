open Core
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Duplicated code is found by comparing the shape of statements: their
   syntax tree with variable names and literal values left out, so a copy
   with renamed variables or different setpoints still matches.

   Each statement list (a POU body, IF/CASE branches, loop bodies) is a block.
   Runs of consecutive statements with the same shapes in two blocks, or in
   two non-overlapping parts of one block, are clones. Only maximal runs are
   kept, and clones contained in bigger ones are dropped.

   The variable names in two copies are aligned by position. When the renamed
   names follow one substitution (P1 -> P2), a name that should follow it but
   doesn't is reported as a likely copy-paste error. *)

(** A statement summarized for comparison. *)
type stmt_info = {
  shape : string;
  size : int; (** Number of syntax tree nodes *)
  names : (string * TI.t) list; (** Variable uses in traversal order *)
  ti : TI.t;
  first_line : int;
  last_line : int;
}

type block = { pou : string; stmts : stmt_info array }

(* {{{ Shapes *)
type acc = {
  buf : Buffer.t;
  mutable size : int;
  mutable rev_names : (string * TI.t) list;
  mutable first : int;
  mutable last : int;
  mutable start : TI.t option; (** Earliest position *)
}

(** A syntax tree node. *)
let node acc tok =
  Buffer.add_string acc.buf tok;
  Buffer.add_char acc.buf ' ';
  acc.size <- acc.size + 1

(** Structure that doesn't count towards the size. *)
let mark acc tok =
  Buffer.add_string acc.buf tok;
  Buffer.add_char acc.buf ' '

let see acc (ti : TI.t) =
  if ti.linenr > 0 then begin
    acc.first <- min acc.first ti.linenr;
    acc.last <- max acc.last ti.linenr;
    match acc.start with
    | Some (s : TI.t) when s.linenr < ti.linenr || (s.linenr = ti.linenr && s.col <= ti.col) -> ()
    | _ -> acc.start <- Some ti
  end

let name acc n ti =
  node acc "V";
  see acc ti;
  acc.rev_names <- (n, ti) :: acc.rev_names

let var_name v =
  match S.VarUse.get_loc v with
  | S.VarUse.DirVar dv -> S.DirVar.get_name dv
  | S.VarUse.SymVar _ -> S.VarUse.get_name v

let const_kind = function
  | S.CInteger _ | S.CBitString _ -> "int"
  | S.CReal _ -> "real"
  | S.CBool _ -> "bool"
  | S.CString _ -> "string"
  | S.CPointer _ -> "pointer"
  | S.CTimeValue _ -> "time"
  | S.CRange _ -> "range"
  | S.CEnumValue _ -> "enum"

let rec expr acc e =
  see acc (S.expr_get_ti e);
  match e with
  | S.ExprVariable (_, v) -> name acc (var_name v) (S.VarUse.get_ti v)
  | S.ExprConstant (_, c) -> node acc ("C:" ^ const_kind c)
  | S.ExprBin (_, l, op, r) ->
    node acc ("(" ^ S.show_operator op); expr acc l; expr acc r; mark acc ")"
  | S.ExprUn (_, op, e) ->
    node acc ("(u" ^ S.show_operator op); expr acc e; mark acc ")"
  | S.ExprFuncCall (_, S.StmFuncCall (_, f, params)) ->
    (* Function names are part of the code, not of the data. *)
    node acc ("(call:" ^ S.Function.get_name f);
    List.iter params ~f:(param acc);
    mark acc ")"
  | S.ExprFuncCall (_, s) -> node acc "(call"; stmt acc s; mark acc ")"

and param acc (p : S.func_param_assign) =
  mark acc ("P:" ^ Option.value p.name ~default:"" ^ if p.inverted then "!" else "");
  stmt acc p.stmt

and stmts acc tag ss =
  mark acc tag;
  List.iter ss ~f:(stmt acc);
  mark acc "."

and stmt acc s =
  see acc (S.stmt_get_ti s);
  match s with
  | S.StmExpr (_, e) -> expr acc e
  | S.StmElsif (_, c, body) -> node acc "ELSIF"; stmt acc c; stmts acc "THEN" body
  | S.StmIf (_, c, body, elsifs, els) ->
    node acc "IF"; stmt acc c; stmts acc "THEN" body;
    List.iter elsifs ~f:(stmt acc); stmts acc "ELSE" els
  | S.StmCase (_, c, sels, els) ->
    node acc "CASE"; stmt acc c;
    List.iter sels ~f:(fun (sel : S.case_selection) ->
        stmts acc "LABELS" sel.case; stmts acc "DO" sel.body);
    stmts acc "ELSE" els
  | S.StmFor (_, ctrl, body) ->
    node acc "FOR"; stmt acc ctrl.assign; expr acc ctrl.range_end;
    expr acc ctrl.range_step; stmts acc "DO" body
  | S.StmWhile (_, c, body) -> node acc "WHILE"; stmt acc c; stmts acc "DO" body
  | S.StmRepeat (_, body, c) -> node acc "REPEAT"; stmts acc "DO" body; stmt acc c
  | S.StmFuncCall (_, f, params) ->
    (* Usually a function block instance, which differs between copies. *)
    node acc "(fbcall";
    name acc (S.Function.get_name f) (S.Function.get_ti f);
    List.iter params ~f:(param acc);
    mark acc ")"
  | S.StmExit _ -> node acc "EXIT"
  | S.StmContinue _ -> node acc "CONTINUE"
  | S.StmReturn _ -> node acc "RETURN"
  | S.StmEmpty _ -> ()

let info_of s =
  let acc = { buf = Buffer.create 64; size = 0; rev_names = [];
              first = Int.max_value; last = 0; start = None } in
  stmt acc s;
  { shape = Buffer.contents acc.buf; size = acc.size; names = List.rev acc.rev_names;
    ti = Option.value acc.start ~default:(S.stmt_get_ti s);
    first_line = acc.first; last_line = acc.last }
(* }}} *)

(* {{{ Blocks *)
let nested = function
  | S.StmIf (_, _, body, elsifs, els) ->
    (body :: List.filter_map elsifs ~f:(function
         | S.StmElsif (_, _, b) -> Some b
         | _ -> None)) @ [els]
  | S.StmCase (_, _, sels, els) -> List.map sels ~f:(fun (sel : S.case_selection) -> sel.body) @ [els]
  | S.StmFor (_, _, b) | S.StmWhile (_, _, b) | S.StmRepeat (_, b, _) -> [b]
  | _ -> []

let rec collect_blocks pou ss blocks =
  let ss = List.filter ss ~f:(function S.StmEmpty _ -> false | _ -> true) in
  let blocks = { pou; stmts = Array.of_list (List.map ss ~f:info_of) } :: blocks in
  List.fold ss ~init:blocks ~f:(fun blocks s ->
      List.fold (nested s) ~init:blocks ~f:(fun blocks b -> collect_blocks pou b blocks))

let pou_bodies elements =
  List.concat_map elements ~f:(function
      | S.IECProgram (_, p) -> [(p.name, p.statements)]
      | S.IECFunctionBlock (_, fb) -> [(S.FunctionBlock.get_name fb.id, fb.statements)]
      | S.IECFunction (_, f) -> [(S.Function.get_name f.id, f.statements)]
      | _ -> [])
(* }}} *)

(* {{{ Clones *)
type range = { pou : string; first : int; last : int; start_ti : TI.t }

type clone = {
  a : range;
  b : range;
  a_names : (string * TI.t) list;
  b_names : (string * TI.t) list;
}

let range_of (blk : block) pos len =
  let ss = Array.sub blk.stmts ~pos ~len in
  { pou = blk.pou;
    first = Array.fold ss ~init:Int.max_value ~f:(fun m s -> min m s.first_line);
    last = Array.fold ss ~init:0 ~f:(fun m s -> max m s.last_line);
    start_ti = ss.(0).ti }

let names_of (blk : block) pos len =
  List.concat_map (Array.to_list (Array.sub blk.stmts ~pos ~len)) ~f:(fun s -> s.names)

(** Length of the unit that [run] repeats, or of [run] if it doesn't repeat.
    Code copied several times in a row matches itself at every multiple of
    the copy's length; only one copy is a clone. The same goes for a list of
    statements of one shape, such as assignments of literals. *)
let period (run : stmt_info array) =
  let n = Array.length run in
  let repeats d =
    let rec go i = i + d >= n || (String.equal run.(i).shape run.(i + d).shape && go (i + 1)) in
    go 0
  in
  let rec find d = if d > n / 2 then n else if repeats d then d else find (d + 1) in
  find 1

(** Maximal runs of statements with equal shapes and at least [min_size]
    nodes. *)
let find_clones min_size (blocks : block array) =
  let by_shape = String.Table.create () in
  Array.iteri blocks ~f:(fun bi blk ->
      Array.iteri blk.stmts ~f:(fun si s ->
          Hashtbl.add_multi by_shape ~key:s.shape ~data:(bi, si)));
  let shape bi si = blocks.(bi).stmts.(si).shape in
  let clones = ref [] in
  Hashtbl.iter by_shape ~f:(fun positions ->
      let positions = List.sort positions ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare) in
      List.iteri positions ~f:(fun i (bp, sp) ->
          List.iter (List.drop positions (i + 1)) ~f:(fun (bq, sq) ->
              let same_block = Int.equal bp bq in
              (* Runs only start where they can't be extended to the left. *)
              let extends_left =
                sp > 0 && sq > 0 && String.equal (shape bp (sp - 1)) (shape bq (sq - 1))
              in
              if not extends_left then begin
                let len_p = Array.length blocks.(bp).stmts
                and len_q = Array.length blocks.(bq).stmts in
                let rec extend n =
                  if sp + n < len_p && sq + n < len_q
                     && (not same_block || sp + n < sq)
                     && String.equal (shape bp (sp + n)) (shape bq (sq + n))
                  then extend (n + 1) else n
                in
                let n = period (Array.sub blocks.(bp).stmts ~pos:sp ~len:(extend 0)) in
                let run = Array.sub blocks.(bp).stmts ~pos:sp ~len:n in
                let size = Array.fold run ~init:0 ~f:(fun acc s -> acc + s.size) in
                if n > 0 && size >= min_size then
                  clones := { a = range_of blocks.(bp) sp n; b = range_of blocks.(bq) sq n;
                              a_names = names_of blocks.(bp) sp n;
                              b_names = names_of blocks.(bq) sq n } :: !clones
              end)));
  (* Order each pair so that [a] comes first. *)
  List.map !clones ~f:(fun c ->
      if c.b.first < c.a.first
      then { a = c.b; b = c.a; a_names = c.b_names; b_names = c.a_names }
      else c)

let contains r1 r2 = r1.first <= r2.first && r2.last <= r1.last

let same_range r1 r2 = Int.equal r1.first r2.first && Int.equal r1.last r2.last

(** Drop clones whose both copies lie inside the copies of a bigger clone,
    e.g. the bodies of two duplicated IF statements. *)
let maximal clones =
  List.filter clones ~f:(fun c ->
      not (List.exists clones ~f:(fun d ->
          not (same_range c.a d.a && same_range c.b d.b)
          && ((contains d.a c.a && contains d.b c.b)
              || (contains d.a c.b && contains d.b c.a)))))
(* }}} *)

(* {{{ Warnings *)
let lines r =
  if Int.equal r.first r.last then Printf.sprintf "line %d" r.first
  else Printf.sprintf "lines %d-%d" r.first r.last

let in_pou (c : clone) =
  if String.equal c.a.pou c.b.pou then "" else Printf.sprintf " in %s" c.a.pou

let duplicate_warnings clones =
  (* A copy is reported once, against its first occurrence. *)
  List.sort clones ~compare:(fun c d -> Int.compare c.a.first d.a.first)
  |> List.fold ~init:[] ~f:(fun kept c ->
      if List.exists kept ~f:(fun k -> same_range k.b c.b) then kept else c :: kept)
  |> List.sort ~compare:(fun c d -> Int.compare c.b.first d.b.first)
  |> List.map ~f:(fun c ->
      let text =
        Printf.sprintf "%s %s %s%s"
          (String.capitalize (lines c.b))
          (if Int.equal c.b.first c.b.last then "duplicates" else "duplicate")
          (lines c.a) (in_pou c)
      in
      Warn.mk c.b.start_ti.linenr c.b.start_ti.col "DuplicateCode" text)

let is_word_char c = Char.is_alphanum c

(** The renamed part of [a] in [b], widened to whole words:
    [P1_RUN], [P2_RUN] -> [P1], [P2]. *)
let substitution a b =
  let la = String.length a and lb = String.length b in
  let rec prefix i = if i < la && i < lb && Char.equal a.[i] b.[i] then prefix (i + 1) else i in
  let p = prefix 0 in
  let rec suffix i =
    if i < la - p && i < lb - p && Char.equal a.[la - 1 - i] b.[lb - 1 - i]
    then suffix (i + 1) else i
  in
  let s = suffix 0 in
  let rec left i = if i > 0 && is_word_char a.[i - 1] then left (i - 1) else i in
  let start = left p in
  let rec right i = if i > 0 && is_word_char a.[la - i] then right (i - 1) else i in
  let s = right s in
  (String.sub a ~pos:start ~len:(la - s - start), String.sub b ~pos:start ~len:(lb - s - start))

(** Replace the occurrences of [from] in [s] that are whole words. Returns
    [None] if there are none. *)
let replace_word s ~from ~to_ =
  let n = String.length s and k = String.length from in
  let boundary i = i < 0 || i >= n || not (is_word_char s.[i]) in
  let buf = Buffer.create n in
  let rec go i found =
    if i > n - k then begin
      Buffer.add_string buf (String.drop_prefix s i);
      found
    end
    else if String.equal (String.sub s ~pos:i ~len:k) from && boundary (i - 1) && boundary (i + k)
    then (Buffer.add_string buf to_; go (i + k) true)
    else (Buffer.add_char buf s.[i]; go (i + 1) found)
  in
  if k > 0 && go 0 false then Some (Buffer.contents buf) else None

(** Names in [dst] that don't follow the substitution the rest of the copy
    of [src] follows. *)
let inconsistent_names src dst =
  let pairs = List.zip_exn src dst in
  let renamed =
    List.filter_map pairs ~f:(fun ((a, _), (b, _)) ->
        if String.equal a b then None else Some (a, b))
    |> List.dedup_and_sort ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
  in
  let counts = Hashtbl.Poly.create () in
  List.iter renamed ~f:(fun (a, b) -> Hashtbl.incr counts (substitution a b));
  let best =
    Hashtbl.fold counts ~init:None ~f:(fun ~key ~data best ->
        match best with
        | Some (_, n) when n >= data -> best
        | _ -> Some (key, data))
  in
  match best with
  | Some ((from, to_), n) when n >= 2 && 2 * n > List.length renamed ->
    List.filter_map pairs ~f:(fun ((a, _), (b, ti)) ->
        match replace_word a ~from ~to_ with
        | Some expected when not (String.equal b expected) -> Some (b, ti, from, to_, expected)
        | _ -> None)
  | _ -> []

let inconsistent_warnings clones =
  let seen = String.Hash_set.create () in
  List.concat_map clones ~f:(fun c ->
      let report orig copy names =
        List.filter_map names ~f:(fun (b, (ti : TI.t), from, to_, expected) ->
            let key = Printf.sprintf "%d:%d" ti.linenr ti.col in
            if Hash_set.mem seen key then None
            else begin
              Hash_set.add seen key;
              let text =
                Printf.sprintf
                  "%s may be a copy-paste error: this copy of %s%s renames %s to %s, so %s was expected"
                  b (lines orig) (if String.equal orig.pou copy.pou then "" else " in " ^ orig.pou)
                  from to_ expected
              in
              Some (Warn.mk ti.linenr ti.col "InconsistentCopy" text)
            end)
      in
      report c.a c.b (inconsistent_names c.a_names c.b_names)
      @ report c.b c.a (inconsistent_names c.b_names c.a_names))
(* }}} *)

let run ?min_size ~duplicates ~inconsistent elements =
  let min_size = Option.value min_size ~default:(Config.duplicate_code_size ()) in
  let blocks =
    List.fold (pou_bodies elements) ~init:[] ~f:(fun blocks (pou, ss) ->
        collect_blocks pou ss blocks)
    |> List.rev |> Array.of_list
  in
  let clones = maximal (find_clones min_size blocks) in
  (if duplicates then duplicate_warnings clones else [])
  @ (if inconsistent then inconsistent_warnings clones else [])
