open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

(* Report variables read before they are initialized, i.e. before they are
   assigned on every path from the start of the POU, when they have no
   initial value. This happens in the first cycle whatever later cycles do.

   Exempt, as in the rule or because they are initialized elsewhere: inputs,
   in-outs, externals and globals; RETAIN and CONSTANT variables; variables
   at physical inputs; function block instances, including of types not
   declared in the analyzed code; references and pointers, which are NULL;
   and variables of types with a default initial value. Writing an element or a member counts as
   initializing the variable. *)

let std_fbs = ["TON"; "TOF"; "TP"; "CTU"; "CTD"; "CTUD"; "R_TRIG"; "F_TRIG"; "SR"; "RS"]

(** Whether values of the type named [ty] have a default initial value, or
    are function block instances. *)
let rec has_default types fbs depth ty =
  List.mem std_fbs ty ~equal:String.equal
  || Set.mem fbs ty
  || (depth < 16 &&
      match Map.find types ty with
      | Some (S.DTyDeclSingleElement (S.DTySpecSimple base, None)) -> has_default types fbs (depth + 1) base
      | Some (S.DTyDeclSingleElement (_, Some _)) -> true
      | Some (S.DTyDeclEnumType (_, _, Some _)) -> true
      | Some (S.DTyDeclSubrange _) -> true (* the lower bound by default *)
      | Some (S.DTyDeclStructType (_, elems)) ->
        List.for_all elems ~f:(fun (e : S.struct_elem_spec) -> Option.is_some e.struct_elem_init_value)
      | _ -> false)

let needs_init types fbs d =
  let local =
    match S.VarDecl.get_attr d with
    | Some (S.VarDecl.Var None | S.VarDecl.Var (Some S.VarDecl.QNonRetain)
           | S.VarDecl.VarOut None | S.VarDecl.VarOut (Some S.VarDecl.QNonRetain)
           | S.VarDecl.VarTemp) -> true
    | None -> true
    | _ -> false
  in
  let at_input =
    match S.VarDecl.get_located_at d with
    | Some dv -> (match S.DirVar.get_loc dv with Some S.DirVar.LocI -> true | _ -> false)
    | None -> false
  in
  let typed_default =
    match S.VarDecl.get_ty_spec d with
    (* A type that isn't declared here is most likely a function block from a
       library or another file, which initializes its instances. *)
    | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) ->
      not (Map.mem types ty) || has_default types fbs 0 ty
    (* References and pointers are NULL until assigned. *)
    | Some (S.DTyDeclRefType _) -> true
    | _ -> false
  in
  local && not (S.VarDecl.get_was_init d) && not at_input && not typed_default

(* {{{ Definite assignment *)
let base_name v =
  let name = S.VarUse.get_name v in
  Option.value_map (String.lsplit2 name ~on:'.') ~default:name ~f:fst

(** Variables read by [e], with their positions. *)
let rec reads e =
  match e with
  | S.ExprVariable (_, v) ->
    (base_name v, S.VarUse.get_ti v) :: List.concat_map (S.index_exprs v) ~f:reads
  | S.ExprConstant _ -> []
  | S.ExprBin (_, l, _, r) -> reads l @ reads r
  | S.ExprUn (_, _, e) -> reads e
  | S.ExprFuncCall (_, S.StmFuncCall (_, _, params)) -> List.concat_map params ~f:param_reads
  | S.ExprFuncCall _ -> []

and param_reads (p : S.func_param_assign) =
  match p.stmt with
  | S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) -> reads e
  | S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, _)) -> []
  | S.StmExpr (_, e) -> reads e
  | _ -> []

let param_writes (p : S.func_param_assign) =
  match p.stmt with
  | S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) -> [base_name v]
  | _ -> []

(** Variables assigned on the current path; [None] if it is unreachable. *)
type state = String.Set.t option

let join (a : state) (b : state) : state =
  match a, b with
  | None, s | s, None -> s
  | Some x, Some y -> Some (Set.inter x y)

let int_const = function
  | S.ExprConstant (_, S.CInteger (_, _, v)) -> Some v
  | S.ExprUn (_, S.NEG, S.ExprConstant (_, S.CInteger (_, _, v))) -> Some (-v)
  | _ -> None

(** A FOR loop whose constant bounds make its body run at least once. *)
let runs_once (ctrl : S.for_control) =
  let start = match ctrl.assign with
    | S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) -> int_const e
    | _ -> None
  in
  let step = Option.value (int_const ctrl.range_step) ~default:1 in
  match start, int_const ctrl.range_end with
  | Some a, Some b -> (step > 0 && a <= b) || (step < 0 && a >= b)
  | _ -> false

let check_elem types fbs elem =
  let candidates =
    AU.get_var_decls elem
    |> List.filter ~f:(needs_init types fbs)
    |> List.map ~f:S.VarDecl.get_var_name
    |> String.Set.of_list
  in
  let reported = String.Table.create () in
  let read assigned (name, ti) =
    if Set.mem candidates name && not (Set.mem assigned name) && not (Hashtbl.mem reported name)
    then Hashtbl.set reported ~key:name ~data:ti
  in
  let rec walk (st : state) stmt : state =
    match st with
    | None -> None
    | Some assigned -> begin
        let reading e = List.iter (reads e) ~f:(read assigned) in
        let cond = function S.StmExpr (_, e) -> reading e | _ -> () in
        match stmt with
        | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, lhs), (S.ASSIGN | S.ASSIGN_REF), rhs)) ->
          reading rhs;
          List.iter (List.concat_map (S.index_exprs lhs) ~f:reads) ~f:(read assigned);
          Some (Set.add assigned (base_name lhs))
        | S.StmExpr (_, e) -> reading e; st
        | S.StmFuncCall (_, _, params) ->
          List.iter (List.concat_map params ~f:param_reads) ~f:(read assigned);
          Some (List.fold (List.concat_map params ~f:param_writes) ~init:assigned ~f:Set.add)
        | S.StmIf (_, c, body, elsifs, els) ->
          (* Each condition is evaluated when the previous ones are false. *)
          cond c;
          let branches =
            body :: List.filter_map elsifs ~f:(function
                | S.StmElsif (_, c, b) -> cond c; Some b
                | _ -> None)
          in
          List.fold branches ~init:(walk_list st els) ~f:(fun acc b -> join acc (walk_list st b))
        | S.StmCase (_, c, sels, els) ->
          cond c;
          List.fold sels ~init:(walk_list st els)
            ~f:(fun acc (sel : S.case_selection) -> join acc (walk_list st sel.body))
        | S.StmFor (_, ctrl, body) ->
          let st = walk st ctrl.assign in
          Option.iter st ~f:(fun a ->
              List.iter (reads ctrl.range_end @ reads ctrl.range_step) ~f:(read a));
          let after_body = walk_list st body in
          (* The body may run zero times, unless constant bounds make it run. *)
          if runs_once ctrl then after_body else st
        | S.StmWhile (_, c, body) ->
          cond c;
          ignore (walk_list st body : state);
          st
        | S.StmRepeat (_, body, c) ->
          let after = walk_list st body in
          Option.iter after ~f:(fun a -> match c with S.StmExpr (_, e) -> List.iter (reads e) ~f:(read a) | _ -> ());
          after
        | S.StmReturn _ -> None
        | S.StmElsif _ | S.StmEmpty _ | S.StmExit _ | S.StmContinue _ -> st
      end
  and walk_list st stmts = List.fold stmts ~init:st ~f:walk in
  ignore (walk_list (Some String.Set.empty) (AU.get_top_stmts elem) : state);
  Hashtbl.to_alist reported
  |> List.map ~f:(fun (name, (ti : Tok_info.t)) ->
      let msg =
        Printf.sprintf
          "Variable %s is read before it is initialized; give it an initial value \
           or assign it first"
          name
      in
      Warn.mk_at ti "PLCOPEN-CP3" msg)
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))
(* }}} *)

let do_check elems =
  let types =
    List.filter_map elems ~f:(function
        | S.IECType (_, _, (name, spec)) -> Some (name, spec)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let fbs =
    List.filter_map elems ~f:(function
        | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id)
        | _ -> None)
    |> String.Set.of_list
  in
  List.concat_map elems ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem types fbs e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP3";
  name = "Variables shall be initialized before being used";
  summary = "A variable without an initial value shall be assigned before it is read.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP3";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
