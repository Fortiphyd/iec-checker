open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn

(* Untrusted values are read from physical inputs (%I) and network-writable
   memory (%M). A finding is an assignment of such a value to an output (%Q)
   without a trusted lower and upper bound applied on the way.

   Statements are walked in order, tracking the taint of each variable. At
   the end of IF/CASE branches the states are joined; loops are iterated to a
   fixpoint. *)

(** Taint of a value: the untrusted variables it derives from, and whether a
    trusted lower/upper bound has been applied since. A value is clean when it
    has no sources or is bounded on both sides. *)
type taint = { srcs : String.Set.t; lo : bool; hi : bool }

let clean = { srcs = String.Set.empty; lo = true; hi = true }

let normalize t =
  if Set.is_empty t.srcs || (t.lo && t.hi) then clean else t

let is_clean t = Set.is_empty t.srcs

let raw name = { srcs = String.Set.singleton name; lo = false; hi = false }

let taint_equal a b =
  Set.equal a.srcs b.srcs && Bool.equal a.lo b.lo && Bool.equal a.hi b.hi

(** Least upper bound: union of sources, bounded only if bounded in both. *)
let join a b =
  normalize { srcs = Set.union a.srcs b.srcs; lo = a.lo && b.lo; hi = a.hi && b.hi }

(** Result of an operation that doesn't preserve bounds. *)
let unbounded ts =
  let srcs = String.Set.union_list (List.map ts ~f:(fun t -> t.srcs)) in
  normalize { srcs; lo = false; hi = false }

type ctx = {
  sources : String.Set.t; (** Variables declared at %I or %M *)
  sinks : String.Set.t; (** Variables declared at %Q *)
  arrays : String.Set.t;
  (** The parser drops indexes when reading array elements, so every access to
      these is treated as an element access. *)
  findings : (TI.t * string * String.Set.t) String.Table.t;
  (** Keyed by position of the sink assignment *)
}

(** Taint of a variable that has no entry in the state. *)
let default ctx name = if Set.mem ctx.sources name then raw name else clean

let lookup ctx m name =
  match Map.find m name with Some t -> t | None -> default ctx name

(** Variable taints on the current path; [None] if the path is unreachable. *)
type state = taint String.Map.t option

let join_state ctx (a : state) (b : state) : state =
  match a, b with
  | None, s | s, None -> s
  | Some m1, Some m2 ->
    Some (Map.merge m1 m2 ~f:(fun ~key -> function
        | `Both (t1, t2) -> Some (join t1 t2)
        | `Left t | `Right t -> Some (join t (default ctx key))))

let state_equal (a : state) (b : state) = Option.equal (Map.equal taint_equal) a b

(* {{{ Variables *)
let is_bit dv =
  match S.DirVar.get_size dv with
  | Some S.DirVar.SizeX | Some S.DirVar.SizeNone -> true
  | _ -> false

(* BOOL inputs are skipped: a bounds check doesn't apply to them. *)
let is_untrusted_dir dv =
  match S.DirVar.get_loc dv with
  | Some S.DirVar.LocI | Some S.DirVar.LocM -> not (is_bit dv)
  | _ -> false

let is_output_dir dv =
  match S.DirVar.get_loc dv with Some S.DirVar.LocQ -> true | _ -> false

type var_ref = {
  name : string;
  partial : bool; (** Array element or struct member *)
  untrusted : bool;
  output : bool;
}

let resolve ctx v =
  match S.VarUse.get_loc v with
  | S.VarUse.DirVar dv ->
    { name = S.DirVar.get_name dv; partial = false;
      untrusted = is_untrusted_dir dv; output = is_output_dir dv }
  | S.VarUse.SymVar sv ->
    let full = S.VarUse.get_name v in
    let name, member =
      match String.lsplit2 full ~on:'.' with
      | Some (hd, _) -> hd, true
      | None -> full, false
    in
    { name;
      partial = member || Set.mem ctx.arrays name
                || not (List.is_empty (S.SymVar.get_array_indexes sv));
      untrusted = Set.mem ctx.sources name;
      output = Set.mem ctx.sinks name }
(* }}} *)

(* {{{ Expressions *)
let bounded_below t = t.lo
let bounded_above t = t.hi

(** [LIMIT(mn, x, mx)] is bounded by whichever limits are trusted. *)
let limit mn x mx =
  normalize { srcs = String.Set.union_list [mn.srcs; x.srcs; mx.srcs];
              lo = is_clean mn; hi = is_clean mx }

(** [MIN] is bounded above by any bounded argument, below only if all are. *)
let min_of ts =
  normalize { srcs = String.Set.union_list (List.map ts ~f:(fun t -> t.srcs));
              lo = List.for_all ts ~f:bounded_below;
              hi = List.exists ts ~f:bounded_above }

let max_of ts =
  normalize { srcs = String.Set.union_list (List.map ts ~f:(fun t -> t.srcs));
              lo = List.exists ts ~f:bounded_below;
              hi = List.for_all ts ~f:bounded_above }

let read ctx m r =
  if r.untrusted then Option.value (Map.find m r.name) ~default:(raw r.name)
  else lookup ctx m r.name

let rec eval ctx m = function
  | S.ExprConstant _ -> clean
  | S.ExprVariable (_, v) -> read ctx m (resolve ctx v)
  (* Comparisons yield BOOL, which isn't tracked. *)
  | S.ExprBin (_, _, (S.GT | S.LT | S.GE | S.LE | S.EQ | S.NEQ), _) -> clean
  | S.ExprBin (_, l, _, r) -> unbounded [eval ctx m l; eval ctx m r]
  | S.ExprUn (_, _, e) -> unbounded [eval ctx m e]
  | S.ExprFuncCall (_, S.StmFuncCall (_, f, params)) ->
    call ctx m (S.Function.get_name f) params
  | S.ExprFuncCall (_, stmt) ->
    unbounded (List.map (AU.get_stmt_exprs stmt) ~f:(eval ctx m))

and call ctx m fname params =
  let args =
    List.filter_map params ~f:(fun (p : S.func_param_assign) ->
        match p.name, p.stmt with
        | Some n, S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) -> Some (Some n, eval ctx m e)
        | None, S.StmExpr (_, e) -> Some (None, eval ctx m e)
        | _ -> None (* output parameters *))
  in
  let ts = List.map args ~f:snd in
  let named = List.exists args ~f:(fun (n, _) -> Option.is_some n) in
  let by_name n = List.Assoc.find args ~equal:(Option.equal String.equal) (Some n) in
  match fname, ts with
  | "LIMIT", [mn; x; mx] when not named -> limit mn x mx
  | "LIMIT", _ -> begin
      match by_name "MN", by_name "IN", by_name "MX" with
      | Some mn, Some x, Some mx -> limit mn x mx
      | _ -> unbounded ts
    end
  | "MIN", _ :: _ -> min_of ts
  | "MAX", _ :: _ -> max_of ts
  | _ -> unbounded ts
(* }}} *)

(* {{{ Conditions *)
let flip = function
  | S.GT -> S.LT
  | S.LT -> S.GT
  | S.GE -> S.LE
  | S.LE -> S.GE
  | op -> op

(** Bound [v] by the fact that [v op bound] is [holds], for a trusted bound. *)
let bound_var ctx m v op holds =
  let lower, upper =
    match op, holds with
    | (S.GT | S.GE), true | (S.LT | S.LE), false -> true, false
    | (S.LT | S.LE), true | (S.GT | S.GE), false -> false, true
    | S.EQ, true | S.NEQ, false -> true, true
    | _ -> false, false
  in
  let r = resolve ctx v in
  let t = read ctx m r in
  (* A bound on one element doesn't bound the whole array or struct. *)
  if r.partial || is_clean t || not (lower || upper) then m
  else Map.set m ~key:r.name
      ~data:(normalize { t with lo = t.lo || lower; hi = t.hi || upper })

(** Refine [st] with the bounds implied by [cond] evaluating to [holds]. *)
let rec refine ctx (st : state) cond holds : state =
  match st with
  | None -> None
  | Some m -> begin
      match cond with
      | S.ExprUn (_, S.NEG, e) -> refine ctx st e (not holds)
      | S.ExprBin (_, l, S.AND, r) when holds -> refine ctx (refine ctx st l true) r true
      | S.ExprBin (_, l, S.OR, r) when not holds -> refine ctx (refine ctx st l false) r false
      | S.ExprBin (_, l, S.AND, r) ->
        join_state ctx (refine ctx st l false) (refine ctx st r false)
      | S.ExprBin (_, l, S.OR, r) ->
        join_state ctx (refine ctx st l true) (refine ctx st r true)
      | S.ExprBin (_, S.ExprVariable (_, v), op, bound) when is_clean (eval ctx m bound) ->
        Some (bound_var ctx m v op holds)
      | S.ExprBin (_, bound, op, S.ExprVariable (_, v)) when is_clean (eval ctx m bound) ->
        Some (bound_var ctx m v (flip op) holds)
      | _ -> st
    end

let refine_stmt ctx st cond holds =
  match cond with
  | S.StmExpr (_, e) -> refine ctx st e holds
  | _ -> st
(* }}} *)

(* {{{ Statements *)
let record ctx lhs r t =
  let ti = S.VarUse.get_ti lhs in
  let key = Printf.sprintf "%d:%d" ti.linenr ti.col in
  Hashtbl.update ctx.findings key ~f:(function
      | Some (ti, name, srcs) -> (ti, name, Set.union srcs t.srcs)
      | None -> (ti, r.name, t.srcs))

let assign ctx m lhs t =
  let r = resolve ctx lhs in
  if r.output && not (is_clean t) then record ctx lhs r t;
  (* The network can still write untrusted memory, so writes don't clean it. *)
  if r.untrusted then m
  else if r.partial then Map.set m ~key:r.name ~data:(join (lookup ctx m r.name) t)
  else Map.set m ~key:r.name ~data:t

let rec walk ctx (st : state) stmt : state =
  match st with
  | None -> None
  | Some m -> begin
      match stmt with
      | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, lhs), (S.ASSIGN | S.ASSIGN_REF), rhs)) ->
        Some (assign ctx m lhs (eval ctx m rhs))
      | S.StmIf (_, cond, body, elsifs, els) ->
        let branches =
          (cond, body) :: List.filter_map elsifs ~f:(function
              | S.StmElsif (_, c, b) -> Some (c, b)
              | _ -> None)
        in
        (* Each branch is taken when its condition holds and all previous
           ones don't; ELSE when none of them hold. *)
        let rest, taken =
          List.fold branches ~init:(st, None) ~f:(fun (rest, taken) (c, b) ->
              let out = walk_list ctx (refine_stmt ctx rest c true) b in
              (refine_stmt ctx rest c false, join_state ctx taken out))
        in
        join_state ctx taken (walk_list ctx rest els)
      | S.StmCase (_, _, sels, els) ->
        List.fold sels ~init:(walk_list ctx st els)
          ~f:(fun acc (sel : S.case_selection) -> join_state ctx acc (walk_list ctx st sel.body))
      | S.StmFor (_, ctrl, body) -> loop ctx (walk ctx st ctrl.assign) body
      | S.StmWhile (_, _, body) -> loop ctx st body
      | S.StmRepeat (_, body, _) -> loop ctx (walk_list ctx st body) body
      | S.StmReturn _ -> None
      (* Function block calls are not followed yet. *)
      | S.StmExpr _ | S.StmElsif _ | S.StmFuncCall _
      | S.StmEmpty _ | S.StmExit _ | S.StmContinue _ -> st
    end

and walk_list ctx st stmts = List.fold stmts ~init:st ~f:(walk ctx)

and loop ctx st body =
  let next = join_state ctx st (walk_list ctx st body) in
  if state_equal next st then st else loop ctx next body
(* }}} *)

let check_pou elem =
  let decls = AU.get_var_decls elem in
  let names_where f =
    List.filter_map decls ~f:(fun d ->
        if f d then Some (S.VarDecl.get_var_name d) else None)
    |> String.Set.of_list
  in
  let located f d = Option.value_map (S.VarDecl.get_located_at d) ~default:false ~f in
  let ctx = {
    sources = names_where (located is_untrusted_dir);
    sinks = names_where (located is_output_dir);
    arrays = names_where (fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclArrayType _) -> true
        | _ -> false);
    findings = String.Table.create ();
  } in
  ignore (walk_list ctx (Some String.Map.empty) (AU.get_top_stmts elem) : state);
  Hashtbl.data ctx.findings
  |> List.sort ~compare:(fun ((a : TI.t), _, _) ((b : TI.t), _, _) ->
      match Int.compare a.linenr b.linenr with 0 -> Int.compare a.col b.col | c -> c)
  |> List.map ~f:(fun ((ti : TI.t), sink, srcs) ->
      let text =
        Printf.sprintf "Output %s is driven by untrusted %s without a bounds check"
          sink (String.concat ~sep:", " (Set.to_list srcs))
      in
      Warn.mk ti.linenr ti.col "TaintedVariable" text)

let run elements =
  List.fold_left
    elements
    ~f:(fun warns e ->
        let ws = match e with
          | S.IECProgram _ | S.IECFunction _ | S.IECFunctionBlock _ -> check_pou e
          | _ -> []
        in
        warns @ ws)
    ~init:[]
