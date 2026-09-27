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
   literal [IF FALSE] is taken to be such a bypass.

   Functions and function blocks nothing in the application uses are dead
   code too, as are programs no configuration runs. *)

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

(* {{{ Unreferenced POUs *)
(** Names of the types used by a type specification. *)
let spec_types =
  (* A type name can parse as either. *)
  let named = function S.DTySpecSimple ty | S.DTySpecEnum ty -> [ty] | _ -> [] in
  function
  | S.DTyDeclSingleElement (spec, _) -> named spec
  | S.DTyDeclArrayType (_, S.TyDerived (S.DTyUseSingleElement spec), _)
  | S.DTyDeclRefType (_, S.TyDerived (S.DTyUseSingleElement spec), _) -> named spec
  | S.DTyDeclArrayType (_, S.TyDerived (S.DTyUseStructType ty), _)
  | S.DTyDeclRefType (_, S.TyDerived (S.DTyUseStructType ty), _) -> [ty]
  | S.DTyDeclStructType (_, elems) ->
    List.concat_map elems ~f:(fun (e : S.struct_elem_spec) -> named e.struct_elem_ty)
  | _ -> []

let decl_types decls = List.concat_map decls ~f:(fun d ->
    Option.value_map (S.VarDecl.get_ty_spec d) ~default:[] ~f:spec_types)

(** POUs and types referenced by [elem]: the types of its variables and the
    functions it calls, including in expressions and call arguments. *)
let references elem =
  let rec in_expr = function
    | S.ExprFuncCall (_, S.StmFuncCall (_, f, _)) -> [S.Function.get_name f]
    | S.ExprBin (_, a, _, b) -> in_expr a @ in_expr b
    | S.ExprUn (_, _, a) -> in_expr a
    | S.ExprFuncCall _ | S.ExprVariable _ | S.ExprConstant _ -> []
  in
  let calls =
    List.filter_map (AU.get_pou_stmts elem) ~f:(function
        | S.StmFuncCall (_, f, _) -> Some (S.Function.get_name f)
        | _ -> None)
  in
  decl_types (AU.get_var_decls elem) @ calls @ List.concat_map (AU.get_pou_exprs elem) ~f:in_expr

let unreferenced elems =
  let configs = List.filter_map elems ~f:(function S.IECConfiguration (_, c) -> Some c | _ -> None) in
  let programs = List.filter_map elems ~f:(function S.IECProgram (_, p) -> Some p.name | _ -> None) in
  let run =
    List.concat_map configs ~f:(fun c ->
        List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
            List.filter_map r.programs ~f:S.ProgramConfig.get_type_name))
  in
  (* Without a configuration that runs programs, the programs are the entry
     points. Code without any is a library, whose POUs are used elsewhere. *)
  let has_tasks = not (List.is_empty run) in
  if not has_tasks && List.is_empty programs then []
  else begin
    let pous =
      List.filter_map elems ~f:(fun e ->
          match e with
          | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, e)
          | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, e)
          | S.IECProgram (_, p) -> Some (p.name, e)
          | S.IECClass (_, c) -> Some (c.class_name, e)
          | S.IECType (_, _, (name, _)) -> Some (name, e)
          | _ -> None)
      |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
    in
    let refs name =
      match Map.find pous name with
      | Some (S.IECType (_, _, (_, spec))) -> spec_types spec
      | Some (S.IECClass (_, c) as e) -> Option.to_list c.parent_name @ references e
      | Some e -> references e
      | None -> []
    in
    (* Entry points, and the types of global variables. *)
    let roots =
      (if has_tasks then run else programs)
      @ List.concat_map configs ~f:(fun c ->
          decl_types (c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)))
    in
    let rec visit seen = function
      | [] -> seen
      | n :: rest when Set.mem seen n -> visit seen rest
      | n :: rest -> visit (Set.add seen n) (refs n @ rest)
    in
    let used = visit String.Set.empty roots in
    List.filter_map elems ~f:(fun e ->
        let kind = match e with
          | S.IECFunction _ -> Some "Function"
          | S.IECFunctionBlock _ -> Some "Function block"
          | S.IECProgram _ when has_tasks -> Some "Program"
          | _ -> None
        in
        Option.bind kind ~f:(fun kind ->
            Option.bind (S.get_pou_name_as_written e) ~f:(fun (name, ti) ->
                if Set.mem used (String.uppercase name) then None
                else
                  let why =
                    match e with
                    | S.IECProgram _ -> "no configuration runs it"
                    | _ -> "nothing in the application uses it"
                  in
                  Some (Warn.mk_at ti "PLCOPEN-CP2"
                          (Printf.sprintf "All code shall be used in the application: \
                                           %s %s is never used (%s)" kind name why)))))
  end
(* }}} *)

let do_check elems =
  List.concat_map elems ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        fst (check_list (AU.get_top_stmts e))
      | _ -> [])
  @ unreferenced elems

let detector : Detector.t = {
  id = "PLCOPEN-CP2";
  name = "All code shall be used in the application";
  summary = "Dead code is not allowed.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP2";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
