open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

module T = IECCheckerAnalysis.Expr_type

(* Report equality and inequality comparisons of times, including times held
   in integers, which the rule also covers ("even in Integer format").

   Times are durations and points in time, which change continuously and are
   unlikely to be exactly equal to a value. Dates are not included: comparing
   them for equality is meaningful. An integer holds a time if it is converted
   from one, computed from one, or is a variable, a function block output or
   the result of a function assigned such a value. Times converted to reals
   are left to CP8.

   Physical measures in integers, such as raw analog inputs, can't be told
   apart from other integers by their types and aren't reported. *)

let time_names =
  ["TIME"; "LTIME"; "TIME_OF_DAY"; "TOD"; "LTIME_OF_DAY"; "LTOD"; "DATE_AND_TIME"; "DT";
   "LDATE_AND_TIME"; "LDT"]

let is_time_ty = function
  | S.TIME | S.LTIME | S.TIME_OF_DAY | S.TOD | S.LTOD
  | S.DATE_AND_TIME | S.DT | S.LDATE_AND_TIME | S.LDT -> true
  | _ -> false

let is_time env e =
  match T.type_of env e with
  | T.Elem ty -> is_time_ty ty
  | _ -> false

let is_integer env e =
  match T.type_of env e with
  | T.Elem ty -> (match T.num_of ty with Some (T.Signed _ | T.Unsigned _) -> true | _ -> false)
  | T.Int_literal _ -> true
  | T.Real_literal | T.Unknown -> false

let base_name v =
  let name = S.VarUse.get_name v in
  Option.value_map (String.lsplit2 name ~on:'.') ~default:name ~f:fst

let member v =
  Option.map (String.lsplit2 (S.VarUse.get_name v) ~on:'.') ~f:snd

(* {{{ Times held in integers *)
(** Whether [e] is an integer that holds a time. [held v] tells whether the
    variable [v] does, and [returns f] whether the function [f] returns one. *)
let rec int_time env ~held ~returns e =
  let recur = int_time env ~held ~returns in
  match e with
  | S.ExprVariable (_, v) -> held v
  | S.ExprBin (_, a, (S.ADD | S.SUB | S.MUL | S.DIV | S.MOD), b) -> recur a || recur b
  | S.ExprUn (_, S.NEG, a) -> recur a
  | S.ExprFuncCall (_, S.StmFuncCall (_, f, params)) ->
    let name = S.Function.get_name f in
    let args = List.filter_map params ~f:(fun (p : S.func_param_assign) ->
        match p.stmt with
        | S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) -> Some e
        | S.StmExpr (_, (S.ExprBin (_, _, S.SENDTO, _))) -> None
        | S.StmExpr (_, e) -> Some e
        | _ -> None)
    in
    let converted =
      (* TIME_TO_DINT, or a conversion of a time such as TO_DINT(t.ET) *)
      match String.substr_index name ~pattern:"_TO_" with
      | Some i ->
        List.mem time_names (String.prefix name i) ~equal:String.equal
        || List.exists args ~f:(is_time env)
      | None -> String.is_prefix name ~prefix:"TO_" && List.exists args ~f:(is_time env)
    in
    let result_int = is_integer env e in
    (converted && result_int)
    || (result_int && returns name)
    || (match name with
        | "MIN" | "MAX" | "LIMIT" | "SEL" | "MUX" | "MOVE" | "ABS" -> List.exists args ~f:recur
        | _ -> false)
  | S.ExprBin _ | S.ExprUn _ | S.ExprConstant _ | S.ExprFuncCall _ -> false

(** Variables of [elem] assigned integers that hold times, given the outputs
    [outputs ty] of function block types and the functions [returns] that
    return them. *)
let held_vars env ~outputs ~returns elem =
  let instance_types =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let assignments =
    AU.get_pou_exprs elem
    |> List.filter_map ~f:(function
        | S.ExprBin (_, S.ExprVariable (_, v), S.ASSIGN, rhs) -> Some (v, rhs)
        | _ -> None)
  in
  let held set v =
    match member v with
    | Some m -> begin
        match Map.find instance_types (base_name v) with
        | Some ty -> Set.mem (outputs ty) m
        | None -> false
      end
    | None -> Set.mem set (base_name v)
  in
  (* Until nothing is added, since values flow from one variable to another. *)
  let rec fix set =
    let set' =
      List.fold assignments ~init:set ~f:(fun acc (v, rhs) ->
          if Option.is_none (member v)
          && is_integer env (S.ExprVariable (S.VarUse.get_ti v, v))
          && int_time env ~held:(held acc) ~returns rhs
          then Set.add acc (base_name v)
          else acc)
    in
    if Set.length set' = Set.length set then set else fix set'
  in
  let set = fix String.Set.empty in
  (set, held set)

(** For each function block and function declared in [elements], its
    variables that hold times in integers. *)
let pou_held elements =
  let pous =
    List.filter_map elements ~f:(fun e ->
        match e with
        | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, e)
        | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, e)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let memo = String.Table.create () in
  let rec of_pou visiting name =
    match Hashtbl.find memo name, Map.find pous name with
    | Some s, _ -> s
    | None, None -> String.Set.empty
    | None, Some _ when Set.mem visiting name -> String.Set.empty (* recursion *)
    | None, Some elem ->
      let visiting = Set.add visiting name in
      let set, _ =
        held_vars (T.env_of elements elem)
          ~outputs:(of_pou visiting) ~returns:(fun f -> Set.mem (of_pou visiting f) f) elem
      in
      Hashtbl.set memo ~key:name ~data:set;
      set
  in
  of_pou String.Set.empty
(* }}} *)

let check_elem elements pou_held elem =
  let env = T.env_of elements elem in
  let returns f = Set.mem (pou_held f) f in
  let _, held = held_vars env ~outputs:pou_held ~returns elem in
  let int_time = int_time env ~held ~returns in
  (* Comparisons can be nested in other expressions. Call arguments are
     separate expressions in [get_pou_exprs]. *)
  let rec check acc = function
    | S.ExprBin (ti, lhs, op, rhs) ->
      let acc = check (check acc lhs) rhs in
      begin match op with
        | S.EQ | S.NEQ ->
          let msg = "Time and physical measures comparisons shall not be equality or inequality" in
          if is_time env lhs || is_time env rhs then Warn.mk_at ti "PLCOPEN-CP28" msg :: acc
          else if int_time lhs || int_time rhs then
            Warn.mk_at ti "PLCOPEN-CP28" (msg ^ " (the integer holds a time)") :: acc
          else acc
        | _ -> acc
      end
    | S.ExprUn (_, _, e) -> check acc e
    | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> acc
  in
  AU.get_pou_exprs elem |> List.fold ~init:[] ~f:check |> List.rev

let do_check elems =
  let pou_held = pou_held elems in
  List.concat_map elems ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        check_elem elems pou_held e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP28";
  name = "Time and physical measures comparisons shall not be equality or inequality";
  summary =
    "Use range comparisons instead of [=] / [<>] when comparing [TIME] values, \
     including times held in integers.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP28";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
