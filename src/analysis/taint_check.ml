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
   fixpoint. Programs and function blocks keep their variables between scans,
   so their bodies are iterated to a fixpoint too.

   Calls to function blocks and functions declared in the analyzed code use a
   summary of the callee: the taint of its outputs, and the sinks in its body,
   in terms of its inputs. *)

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

(* {{{ Summaries *)
type param_kind = PIn | POut | PInOut

type finding = {
  at : TI.t; (** Position of the sink assignment *)
  sink : string;
  tainted_by : String.Set.t;
  calls : Int.Set.t; (** Lines of the calls that lead to this sink *)
}

type summary = {
  params : (string * param_kind) list; (** In declaration order *)
  results : taint String.Map.t;
  (** Taint of outputs, in-outs and the function result, in terms of inputs *)
  inner : finding list; (** Sinks in the body reachable from inputs *)
}

(** Inputs are represented in summaries as pseudo-sources named [$NAME]. *)
let param_src p = "$" ^ p

let is_param_src n = String.is_prefix n ~prefix:"$"

(** Replace pseudo-sources in [srcs] with the sources of the arguments. *)
let subst_srcs args srcs =
  Set.fold srcs ~init:String.Set.empty ~f:(fun acc n ->
      if is_param_src n then Set.union acc (args (String.drop_prefix n 1)).srcs
      else Set.add acc n)

let subst args t = normalize { t with srcs = subst_srcs args t.srcs }

let timer_summary = {
  params = [("IN", PIn); ("PT", PIn)];
  (* 0 <= ET <= PT *)
  results = String.Map.singleton "ET" { srcs = String.Set.singleton (param_src "PT");
                                        lo = true; hi = false };
  inner = [];
}

(** Standard function blocks. Their BOOL outputs and counters don't carry
    taint from inputs. *)
let std_summary = function
  | "TON" | "TOF" | "TP" -> Some timer_summary
  | "CTU" | "CTD" | "CTUD" | "R_TRIG" | "F_TRIG" | "SR" | "RS" ->
    Some { params = []; results = String.Map.empty; inner = [] }
  | _ -> None
(* }}} *)

type env = {
  globals : S.VarDecl.t String.Map.t; (** Located global variables *)
  fbs : S.fb_decl String.Map.t;
  funcs : S.function_decl String.Map.t;
  summaries : summary option String.Table.t;
  (** Computed summaries; [None] while one is being computed. *)
}

type ctx = {
  env : env;
  sources : String.Set.t; (** Variables declared at %I or %M *)
  sinks : String.Set.t; (** Variables declared at %Q *)
  arrays : String.Set.t;
  (** The parser drops indexes when reading array elements, so every access to
      these is treated as an element access. *)
  unsigned : String.Set.t; (** Variables of unsigned types, which are >= 0 *)
  instances : string String.Map.t; (** Function block instance -> type *)
  findings : finding String.Table.t; (** Keyed by position of the sink *)
  returns : state ref; (** Join of the states at RETURN statements *)
  exits : state ref; (** ... at EXIT statements of the innermost loop *)
  continues : state ref; (** ... at CONTINUE statements of the innermost loop *)
}

(** Variable taints on the current path; [None] if the path is unreachable. *)
and state = taint String.Map.t option

(** Taint of a variable that has no entry in the state. *)
let default ctx name = if Set.mem ctx.sources name then raw name else clean

let lookup ctx m name =
  match Map.find m name with Some t -> t | None -> default ctx name

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
  unsigned : bool;
}

let resolve ctx v =
  match S.VarUse.get_loc v with
  | S.VarUse.DirVar dv ->
    (* Directly represented variables are bit strings (BYTE, WORD, ...). *)
    { name = S.DirVar.get_name dv; partial = false;
      untrusted = is_untrusted_dir dv; output = is_output_dir dv;
      unsigned = not (is_bit dv) }
  | S.VarUse.SymVar sv ->
    let full = S.VarUse.get_name v in
    let indexed = Set.mem ctx.arrays full
                  || not (List.is_empty (S.SymVar.get_array_indexes sv)) in
    match String.lsplit2 full ~on:'.' with
    | Some (inst, rest) when Map.mem ctx.instances inst ->
      (* Members of function block instances are tracked separately. *)
      let member, nested =
        match String.lsplit2 rest ~on:'.' with
        | Some (m, _) -> m, true
        | None -> rest, false
      in
      { name = inst ^ "." ^ member; partial = nested || indexed;
        untrusted = false; output = false; unsigned = false }
    | Some (base, _) ->
      { name = base; partial = true;
        untrusted = Set.mem ctx.sources base; output = Set.mem ctx.sinks base;
        unsigned = false }
    | None ->
      { name = full; partial = indexed;
        untrusted = Set.mem ctx.sources full; output = Set.mem ctx.sinks full;
        unsigned = Set.mem ctx.unsigned full }
(* }}} *)

(* Set once the summary machinery is defined below. *)
let summary_of : (env -> string -> summary option) ref = ref (fun _ _ -> None)

let record ctx (fd : finding) =
  let key = Printf.sprintf "%d:%d" fd.at.linenr fd.at.col in
  Hashtbl.update ctx.findings key ~f:(function
      | Some old -> { old with tainted_by = Set.union old.tainted_by fd.tainted_by;
                               calls = Set.union old.calls fd.calls }
      | None -> fd)

(** Report the sinks in a callee reached by the arguments of a call. *)
let record_inner ctx (sm : summary) args (call_ti : TI.t) =
  List.iter sm.inner ~f:(fun fd ->
      let tainted_by = subst_srcs args (Set.filter fd.tainted_by ~f:is_param_src) in
      if not (Set.is_empty tainted_by) then
        record ctx { fd with tainted_by; calls = Int.Set.singleton call_ti.linenr })

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

let with_type_bound r t =
  if r.unsigned then normalize { t with lo = true } else t

let read ctx m r =
  with_type_bound r
    (if r.untrusted then Option.value (Map.find m r.name) ~default:(raw r.name)
     else lookup ctx m r.name)

let rec eval ctx m = function
  | S.ExprConstant _ -> clean
  | S.ExprVariable (_, v) -> read ctx m (resolve ctx v)
  (* Comparisons yield BOOL, which isn't tracked. *)
  | S.ExprBin (_, _, (S.GT | S.LT | S.GE | S.LE | S.EQ | S.NEQ), _) -> clean
  | S.ExprBin (_, l, _, r) -> unbounded [eval ctx m l; eval ctx m r]
  | S.ExprUn (_, _, e) -> unbounded [eval ctx m e]
  | S.ExprFuncCall (_, S.StmFuncCall (ti, f, params)) ->
    call ctx m ti (S.Function.get_name f) params
  | S.ExprFuncCall (_, stmt) ->
    unbounded (List.map (AU.get_stmt_exprs stmt) ~f:(eval ctx m))

(** Bind the actual parameters of a call to [params]. Returns the input taints
    by parameter name, and the variables receiving outputs and in-outs. *)
and bind ctx m params (actuals : S.func_param_assign list) =
  let kind p = List.Assoc.find params ~equal:String.equal p in
  let in_out_target p e targets =
    match kind p, e with
    | Some PInOut, S.ExprVariable (_, v) -> (p, v) :: targets
    | _ -> targets
  in
  let positional =
    List.filter_map params ~f:(function (p, (PIn | PInOut)) -> Some p | (_, POut) -> None)
  in
  let _, inputs, targets =
    List.fold actuals ~init:(positional, [], [])
      ~f:(fun (pos, inputs, targets) (a : S.func_param_assign) ->
          match a.name, a.stmt with
          | Some p, S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) ->
            (pos, inputs, (p, v) :: targets)
          | Some p, S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) ->
            (pos, (p, eval ctx m e) :: inputs, in_out_target p e targets)
          | None, S.StmExpr (_, e) -> begin
              match pos with
              | p :: rest -> (rest, (p, eval ctx m e) :: inputs, in_out_target p e targets)
              | [] -> (pos, inputs, targets)
            end
          | _ -> (pos, inputs, targets))
  in
  (inputs, targets)

and call ctx m ti fname params =
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
  | _ -> begin
      match !summary_of ctx.env fname with
      | Some sm when Map.mem ctx.env.funcs fname ->
        let inputs, _ = bind ctx m sm.params params in
        let args p = Option.value (List.Assoc.find inputs ~equal:String.equal p) ~default:clean in
        record_inner ctx sm args ti;
        subst args (Option.value (Map.find sm.results fname) ~default:clean)
      | _ -> unbounded ts
    end
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

(** A CASE branch whose labels are all constants bounds the selector. *)
let refine_case ctx m sel_stmt (sel : S.case_selection) : state =
  let is_const = function S.StmExpr (_, S.ExprConstant _) -> true | _ -> false in
  match sel_stmt with
  | S.StmExpr (_, S.ExprVariable (_, v))
    when not (List.is_empty sel.case) && List.for_all sel.case ~f:is_const ->
    Some (bound_var ctx m v S.EQ true)
  | _ -> Some m
(* }}} *)

(* {{{ Statements *)
let assign ctx m lhs t =
  let r = resolve ctx lhs in
  let t = with_type_bound r t in
  if r.output && not (is_clean t) then
    record ctx { at = S.VarUse.get_ti lhs; sink = r.name; tainted_by = t.srcs;
                 calls = Int.Set.empty };
  (* The network can still write untrusted memory, so writes don't clean it. *)
  if r.untrusted then m
  else if r.partial then Map.set m ~key:r.name ~data:(join (lookup ctx m r.name) t)
  else Map.set m ~key:r.name ~data:t

let assign_targets ctx m targets value =
  List.fold targets ~init:m ~f:(fun m (p, v) -> assign ctx m v (value p))

let call_instance ctx m (ti : TI.t) inst (sm : summary) actuals =
  let key p = inst ^ "." ^ p in
  let inputs, targets = bind ctx m sm.params actuals in
  (* Inputs keep their value between calls, so a value passed at one call site
     can still be seen at another. *)
  let m =
    List.fold inputs ~init:m ~f:(fun m (p, t) ->
        Map.set m ~key:(key p) ~data:(join (lookup ctx m (key p)) t))
  in
  let args p = lookup ctx m (key p) in
  record_inner ctx sm args ti;
  let m =
    Map.fold sm.results ~init:m ~f:(fun ~key:o ~data:t m ->
        Map.set m ~key:(key o) ~data:(subst args t))
  in
  assign_targets ctx m targets (fun p -> lookup ctx m (key p))

(** A function called as a statement: only its outputs and in-outs matter. *)
let call_function ctx m (ti : TI.t) (sm : summary) actuals =
  let inputs, targets = bind ctx m sm.params actuals in
  let args p = Option.value (List.Assoc.find inputs ~equal:String.equal p) ~default:clean in
  record_inner ctx sm args ti;
  assign_targets ctx m targets (fun p ->
      subst args (Option.value (Map.find sm.results p) ~default:clean))

(** Unknown callee: every output depends on every input. *)
let call_unknown ctx m name (actuals : S.func_param_assign list) =
  let ts, targets =
    List.fold actuals ~init:([], []) ~f:(fun (ts, targets) (a : S.func_param_assign) ->
        match a.stmt with
        | S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) ->
          (ts, ("", v) :: targets)
        | S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) | S.StmExpr (_, e) ->
          (eval ctx m e :: ts, targets)
        | _ -> (ts, targets))
  in
  let t = join (lookup ctx m name) (unbounded ts) in
  assign_targets ctx (Map.set m ~key:name ~data:t) targets (fun _ -> t)

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
      | S.StmCase (_, sel_stmt, sels, els) ->
        List.fold sels ~init:(walk_list ctx st els)
          ~f:(fun acc (sel : S.case_selection) ->
              join_state ctx acc (walk_list ctx (refine_case ctx m sel_stmt sel) sel.body))
      | S.StmFor (_, ctrl, body) -> loop ctx ~test_first:true (walk ctx st ctrl.assign) body
      | S.StmWhile (_, cond, body) -> loop ctx ~cond:(cond, true) ~test_first:true st body
      | S.StmRepeat (_, body, cond) -> loop ctx ~cond:(cond, false) ~test_first:false st body
      | S.StmReturn _ ->
        ctx.returns := join_state ctx !(ctx.returns) st;
        None
      | S.StmExit _ ->
        ctx.exits := join_state ctx !(ctx.exits) st;
        None
      | S.StmContinue _ ->
        ctx.continues := join_state ctx !(ctx.continues) st;
        None
      | S.StmFuncCall (ti, f, actuals) -> begin
          let name = S.Function.get_name f in
          let sm = function
            | Some ty -> !summary_of ctx.env ty
            | None -> None
          in
          match Map.find ctx.instances name with
          | Some ty -> begin
              match sm (Some ty) with
              | Some s -> Some (call_instance ctx m ti name s actuals)
              | None -> Some (call_unknown ctx m name actuals)
            end
          | None -> begin
              match sm (Option.some_if (Map.mem ctx.env.funcs name) name) with
              | Some s -> Some (call_function ctx m ti s actuals)
              | None -> Some (call_unknown ctx m name actuals)
            end
        end
      | S.StmExpr _ | S.StmElsif _ | S.StmEmpty _ -> st
    end

and walk_list ctx st stmts = List.fold stmts ~init:st ~f:(walk ctx)

(** Walk a loop to a fixpoint and return the state after it. The body runs
    while [cond] evaluates to the given value; [test_first] is false for
    REPEAT, whose body runs before the first test. *)
and loop ctx ?cond ~test_first st body =
  let outer_exits = !(ctx.exits) and outer_continues = !(ctx.continues) in
  ctx.exits := None;
  ctx.continues := None;
  let refine_cond st holds =
    match cond with
    | Some (c, runs) -> refine_stmt ctx st c (Bool.equal holds runs)
    | None -> st
  in
  (* State at the loop test, after the body and at CONTINUE statements. *)
  let run st = join_state ctx (walk_list ctx st body) !(ctx.continues) in
  let rec fix st =
    let next = join_state ctx st (run (refine_cond st true)) in
    if state_equal next st then st else fix next
  in
  let at_test = fix (if test_first then st else run st) in
  let after = join_state ctx (refine_cond at_test false) !(ctx.exits) in
  ctx.exits := outer_exits;
  ctx.continues := outer_continues;
  after
(* }}} *)

(* {{{ POUs *)
type pou = {
  decls : S.VarDecl.t list;
  stmts : S.statement list;
  persistent : bool; (** Variables keep their values between calls *)
  result : string option; (** Name of the function result variable *)
}

let pou_of_fb (fb : S.fb_decl) =
  { decls = fb.variables; stmts = fb.statements; persistent = true; result = None }

let pou_of_func (f : S.function_decl) =
  { decls = f.variables; stmts = f.statements; persistent = false;
    result = Some (S.Function.get_name f.id) }

let pou_of_elem = function
  | S.IECProgram (_, p) ->
    Some { decls = p.variables; stmts = p.statements; persistent = true; result = None }
  | S.IECFunctionBlock (_, fb) -> Some (pou_of_fb fb)
  | S.IECFunction (_, f) -> Some (pou_of_func f)
  | _ -> None

let located_decls decls =
  List.filter decls ~f:(fun d -> Option.is_some (S.VarDecl.get_located_at d))
  |> List.map ~f:(fun d -> (S.VarDecl.get_var_name d, d))
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

let is_unsigned d =
  match S.VarDecl.get_ty_spec d with
  | Some (S.DTyDeclSingleElement (S.DTySpecElementary ty, _)) -> begin
      match ty with
      | S.USINT | S.UINT | S.UDINT | S.ULINT
      | S.BYTE | S.WORD | S.DWORD | S.LWORD -> true
      | _ -> false
    end
  | _ -> false

let is_global d =
  match S.VarDecl.get_attr d with Some (S.VarDecl.VarGlobal _) -> true | _ -> false

(** Located global variables, declared in configurations, resources or
    VAR_GLOBAL blocks of POUs. *)
let collect_globals elements =
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) ->
        c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
      | e -> List.filter (AU.get_var_decls e) ~f:is_global)
  |> located_decls

let mk_ctx env decls findings =
  (* A global is visible unless the POU declares its own variable with that
     name; VAR_EXTERNAL refers to the global. *)
  let shadowed =
    List.filter_map decls ~f:(fun d ->
        match S.VarDecl.get_attr d with
        | Some (S.VarDecl.VarExternal _) -> None
        | _ -> Some (S.VarDecl.get_var_name d))
    |> String.Set.of_list
  in
  let located =
    Map.merge_skewed
      (Map.filter_keys env.globals ~f:(fun n -> not (Set.mem shadowed n)))
      (located_decls decls)
      ~combine:(fun ~key:_ _ local -> local)
  in
  let located_where f =
    Map.filter located ~f:(fun d -> Option.exists (S.VarDecl.get_located_at d) ~f)
    |> Map.keys |> String.Set.of_list
  in
  let typed f =
    List.filter_map decls ~f:(fun d ->
        Option.bind (S.VarDecl.get_ty_spec d) ~f:(f (S.VarDecl.get_var_name d)))
  in
  let is_fb ty = Map.mem env.fbs ty || Option.is_some (std_summary ty) in
  {
    env;
    sources = located_where is_untrusted_dir;
    sinks = located_where is_output_dir;
    arrays = String.Set.of_list (typed (fun name -> function
        | S.DTyDeclArrayType _ -> Some name
        | _ -> None));
    unsigned = Map.data located @ decls
               |> List.filter ~f:is_unsigned
               |> List.map ~f:S.VarDecl.get_var_name
               |> String.Set.of_list;
    instances = String.Map.of_alist_reduce ~f:(fun first _ -> first)
        (typed (fun name -> function
             | S.DTyDeclSingleElement (S.DTySpecSimple ty, _) when is_fb ty -> Some (name, ty)
             | _ -> None));
    findings;
    returns = ref None;
    exits = ref None;
    continues = ref None;
  }

(** Walk the body of [pou] from [init] and return the state at its exit. *)
let analyze ctx pou (init : state) : state =
  let once st =
    ctx.returns := None;
    let out = walk_list ctx st pou.stmts in
    join_state ctx out !(ctx.returns)
  in
  if pou.persistent then begin
    (* Values left at the end of one scan are seen by the next one. *)
    let rec fix st =
      let next = join_state ctx st (once st) in
      if state_equal next st then st else fix next
    in
    once (fix init)
  end
  else once init

let params_of decls =
  List.filter_map decls ~f:(fun d ->
      let kind = match S.VarDecl.get_attr d with
        | Some (S.VarDecl.VarIn _) -> Some PIn
        | Some (S.VarDecl.VarOut _) -> Some POut
        | Some S.VarDecl.VarInOut -> Some PInOut
        | _ -> None
      in
      Option.map kind ~f:(fun k -> (S.VarDecl.get_var_ti d, (S.VarDecl.get_var_name d, k))))
  (* The parser doesn't keep declaration order. *)
  |> List.sort ~compare:(fun ((a : TI.t), _) ((b : TI.t), _) ->
      match Int.compare a.linenr b.linenr with 0 -> Int.compare a.col b.col | c -> c)
  |> List.map ~f:snd

let compute_summary env pou =
  let params = params_of pou.decls in
  let ctx = mk_ctx env pou.decls (String.Table.create ()) in
  let init =
    List.fold params ~init:String.Map.empty ~f:(fun m (p, k) ->
        match k with
        | PIn | PInOut -> Map.set m ~key:p ~data:(raw (param_src p))
        | POut -> m)
  in
  let exit = Option.value (analyze ctx pou (Some init)) ~default:String.Map.empty in
  let outs =
    List.filter_map params ~f:(function (p, (POut | PInOut)) -> Some p | (_, PIn) -> None)
    @ Option.to_list pou.result
  in
  { params;
    results = String.Map.of_alist_reduce ~f:(fun a _ -> a)
        (List.map outs ~f:(fun o -> (o, lookup ctx exit o)));
    inner = Hashtbl.data ctx.findings
            |> List.filter ~f:(fun fd -> Set.exists fd.tainted_by ~f:is_param_src) }

let () =
  summary_of := fun env name ->
    match Hashtbl.find env.summaries name with
    | Some s -> s (* [None] for a recursive call *)
    | None ->
      let pou =
        match Map.find env.fbs name, Map.find env.funcs name with
        | Some fb, _ -> Some (pou_of_fb fb)
        | None, Some f -> Some (pou_of_func f)
        | None, None -> None
      in
      match pou with
      | None -> std_summary name
      | Some pou ->
        Hashtbl.set env.summaries ~key:name ~data:None;
        let s = compute_summary env pou in
        Hashtbl.set env.summaries ~key:name ~data:(Some s);
        Some s
(* }}} *)

let to_warning fd =
  let via =
    match Set.to_list fd.calls with
    | [] -> ""
    | [l] -> Printf.sprintf " (through the call on line %d)" l
    | ls -> Printf.sprintf " (through calls on lines %s)"
              (String.concat ~sep:", " (List.map ls ~f:Int.to_string))
  in
  let text =
    Printf.sprintf "Output %s is driven by untrusted %s without a bounds check%s"
      fd.sink (String.concat ~sep:", " (Set.to_list fd.tainted_by)) via
  in
  Warn.mk_at fd.at "TaintedVariable" text

let run elements =
  let env = {
    globals = collect_globals elements;
    fbs = String.Map.of_alist_reduce ~f:(fun a _ -> a)
        (List.filter_map elements ~f:(function
             | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, fb)
             | _ -> None));
    funcs = String.Map.of_alist_reduce ~f:(fun a _ -> a)
        (List.filter_map elements ~f:(function
             | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, f)
             | _ -> None));
    summaries = String.Table.create ();
  } in
  (* Shared by all POUs: a sink in a function block can be reported both when
     checking the block and through a call. *)
  let findings = String.Table.create () in
  List.iter elements ~f:(fun e ->
      Option.iter (pou_of_elem e) ~f:(fun pou ->
          let ctx = mk_ctx env pou.decls findings in
          ignore (analyze ctx pou (Some String.Map.empty) : state)));
  Hashtbl.data findings
  |> List.sort ~compare:(fun a b ->
      match Int.compare a.at.linenr b.at.linenr with
      | 0 -> Int.compare a.at.col b.at.col
      | c -> c)
  |> List.map ~f:to_warning
