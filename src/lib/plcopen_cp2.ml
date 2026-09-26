open Core
open IECCheckerCore

module TI = Tok_info
module AU = Ast_util
module S = Syntax

(* Report dead code: statements after RETURN, EXIT or CONTINUE, or after a
   statement all of whose branches end with one, and code guarded by a
   condition that is always false (or the other branches of one that is
   always true). Each dead region is reported once, at its first statement.

   The rule allows a bypassed section explained by a comment, and gives
   [IF FALSE THEN] as its example. Comments aren't kept by the parser, so a
   literal [IF FALSE] is taken to be such a bypass. *)

let warn stmt why =
  Warn.mk_at (S.stmt_get_ti stmt) "PLCOPEN-CP2"
    (Printf.sprintf "All code shall be used in the application: unreachable code (%s)" why)

(* {{{ Constant conditions *)
let rec const_int = function
  | S.ExprConstant (_, (S.CInteger (_, _, v) | S.CBitString (_, _, v))) -> Some v
  | S.ExprUn (_, S.NEG, e) -> Option.map (const_int e) ~f:Int.neg
  | S.ExprBin (_, a, op, b) -> begin
      match const_int a, const_int b with
      | Some x, Some y -> begin
          match op with
          | S.ADD -> Some (x + y)
          | S.SUB -> Some (x - y)
          | S.MUL -> Some (x * y)
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None

(** The value of a condition if it is constant. *)
let rec const_bool = function
  | S.ExprConstant (_, S.CBool (_, b)) -> Some b
  | S.ExprUn (_, S.NEG, e) -> Option.map (const_bool e) ~f:not
  | S.ExprBin (_, a, S.AND, b) -> begin
      match const_bool a, const_bool b with
      | Some false, _ | _, Some false -> Some false
      | Some true, Some true -> Some true
      | _ -> None
    end
  | S.ExprBin (_, a, S.OR, b) -> begin
      match const_bool a, const_bool b with
      | Some true, _ | _, Some true -> Some true
      | Some false, Some false -> Some false
      | _ -> None
    end
  | S.ExprBin (_, a, (S.GT | S.LT | S.GE | S.LE | S.EQ | S.NEQ as op), b) -> begin
      match const_int a, const_int b with
      | Some x, Some y ->
        Some (match op with
            | S.GT -> x > y | S.LT -> x < y | S.GE -> x >= y | S.LE -> x <= y
            | S.EQ -> x = y | _ -> x <> y)
      | _ -> None
    end
  | _ -> None

let cond_value = function
  | S.StmExpr (_, e) -> const_bool e
  | _ -> None

let is_literal_false = function
  | S.StmExpr (_, S.ExprConstant (_, S.CBool (_, false))) -> true
  | _ -> false
(* }}} *)

(** Warnings for dead code in [stmts], and whether the list ends with a jump
    on every path, so that code after it is unreachable. *)
let rec check_list stmts : Warn.t list * bool =
  let rec go acc = function
    | [] -> (acc, false)
    | s :: rest ->
      let ws, jumps = check_stmt s in
      let acc = acc @ ws in
      if jumps then
        match List.find rest ~f:(function S.StmEmpty _ -> false | _ -> true) with
        | Some dead -> (acc @ [warn dead "it follows RETURN, EXIT or CONTINUE"], true)
        | None -> (acc, true)
      else go acc rest
  in
  go [] stmts

and dead_body why = function
  | [] -> []
  | first :: _ -> [warn first why]

and check_stmt stmt : Warn.t list * bool =
  match stmt with
  | S.StmReturn _ | S.StmExit _ | S.StmContinue _ -> ([], true)
  | S.StmIf (_, cond, body, elsifs, els) ->
    let branches =
      (cond, body) :: List.filter_map elsifs ~f:(function
          | S.StmElsif (_, c, b) -> Some (c, b)
          | _ -> None)
    in
    (* Walk the branches until one is taken for sure. *)
    let rec go acc all_jump = function
      | [] ->
        let ws, jumps = check_list els in
        (acc @ ws, all_jump && jumps && not (List.is_empty els))
      | (c, b) :: rest -> begin
          match cond_value c with
          | Some false ->
            let ws = if is_literal_false c then [] else dead_body "the condition is always FALSE" b in
            go (acc @ ws) all_jump rest
          | Some true ->
            let ws, jumps = check_list b in
            let later =
              List.concat_map rest ~f:(fun (_, b) -> dead_body "a previous condition is always TRUE" b)
              @ dead_body "a previous condition is always TRUE" els
            in
            (acc @ ws @ later, all_jump && jumps)
          | None ->
            let ws, jumps = check_list b in
            go (acc @ ws) (all_jump && jumps) rest
        end
    in
    go [] true branches
  | S.StmCase (_, _, sels, els) ->
    let ws, all_jump =
      List.fold sels ~init:([], true) ~f:(fun (acc, all) (sel : S.case_selection) ->
          let ws, jumps = check_list sel.body in
          (acc @ ws, all && jumps))
    in
    let ews, ejumps = check_list els in
    (ws @ ews, all_jump && ejumps && not (List.is_empty els))
  | S.StmWhile (_, cond, body) ->
    begin match cond_value cond with
      | Some false -> (dead_body "the loop condition is always FALSE" body, false)
      | _ -> (fst (check_list body), false)
    end
  | S.StmFor (_, ctrl, body) ->
    let start = match ctrl.assign with
      | S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) -> const_int e
      | _ -> None
    in
    let step = Option.value (const_int ctrl.range_step) ~default:1 in
    let empty =
      match start, const_int ctrl.range_end with
      | Some a, Some b -> (step > 0 && a > b) || (step < 0 && a < b)
      | _ -> false
    in
    if empty then (dead_body "the FOR loop never runs" body, false)
    else (fst (check_list body), false)
  | S.StmRepeat (_, body, _) ->
    (* EXIT and CONTINUE in the body belong to this loop. *)
    let ws, jumps = check_list body in
    (ws, jumps && List.exists body ~f:(function S.StmReturn _ -> true | _ -> false))
  | S.StmExpr _ | S.StmFuncCall _ | S.StmElsif _ | S.StmEmpty _ -> ([], false)

let do_check elems =
  List.concat_map elems ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        fst (check_list (AU.get_top_stmts e))
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP2";
  name = "All code shall be used in the application";
  summary = "Dead code is not allowed.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP2";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
