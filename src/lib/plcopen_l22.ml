open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util
module TI = Tok_info

(* Report changes of the control variable of a FOR loop, and of the
   variables its final value and increment are computed from, in the body of
   the loop. The standard says they "shall not be altered by any of the
   repeated statements": whether the final value is evaluated again on each
   iteration depends on the implementation.

   Changes are assignments, outputs of calls ([=>]), arguments passed to
   VAR_IN_OUT parameters of declared POUs, and the control variables of
   nested FOR loops. The initial value is evaluated once, so the variables it
   is computed from may change. *)

(** Whether writing the variable named [written] changes the variable named
    [protected]: one is the other or one of its members. *)
let overlaps written protected =
  let within a b = String.equal a b || String.is_prefix b ~prefix:(a ^ ".") in
  within written protected || within protected written

(** Names of the variables [e] is computed from, including array subscripts. *)
let rec reads = function
  | S.ExprVariable (_, v) -> S.VarUse.get_name v :: List.concat_map (S.index_exprs v) ~f:reads
  | S.ExprBin (_, a, _, b) -> reads a @ reads b
  | S.ExprUn (_, _, e) -> reads e
  | S.ExprFuncCall (_, S.StmFuncCall (_, _, params)) ->
    List.concat_map params ~f:(fun (p : S.func_param_assign) ->
        match p.stmt with
        | S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) | S.StmExpr (_, e) -> reads e
        | _ -> [])
  | S.ExprConstant _ | S.ExprFuncCall _ -> []

(** The statements and all statements nested in them. *)
let rec all_stmts stmt =
  stmt :: (match stmt with
      | S.StmFor (_, _, body) | S.StmElsif (_, _, body) | S.StmWhile (_, _, body)
      | S.StmRepeat (_, body, _) -> List.concat_map body ~f:all_stmts
      | S.StmIf (_, _, body, elsifs, els) -> List.concat_map (body @ elsifs @ els) ~f:all_stmts
      | S.StmCase (_, _, sels, els) ->
        List.concat_map sels ~f:(fun (sel : S.case_selection) -> List.concat_map sel.body ~f:all_stmts)
        @ List.concat_map els ~f:all_stmts
      | S.StmExpr _ | S.StmFuncCall _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _
      | S.StmEmpty _ -> [])

(* {{{ Writes *)
type how = Assigned | Output of string * string | In_out of string * string

type write = { name : string; ti : TI.t; how : how }

(** VAR_IN_OUT parameters of the functions and function blocks declared in
    [elements], by name, with the position of each among the inputs. *)
let in_outs elements =
  List.filter_map elements ~f:(function
      | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, fb.variables)
      | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, f.variables)
      | _ -> None)
  |> List.map ~f:(fun (name, decls) ->
      let inputs =
        List.filter decls ~f:(fun d ->
            match S.VarDecl.get_attr d with
            | Some (S.VarDecl.VarIn _ | S.VarDecl.VarInOut) -> true
            | _ -> false)
        |> List.sort ~compare:(fun a b ->
            let ta = S.VarDecl.get_var_ti a and tb = S.VarDecl.get_var_ti b in
            Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (ta.linenr, ta.col) (tb.linenr, tb.col))
        |> List.map ~f:(fun d ->
            (S.VarDecl.get_var_name d,
             match S.VarDecl.get_attr d with Some S.VarDecl.VarInOut -> true | _ -> false))
      in
      (name, inputs))
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

(** Variables passed to VAR_IN_OUT parameters in a call of [callee]. *)
let in_out_args in_outs callee params =
  match Map.find in_outs callee with
  | None -> []
  | Some inputs ->
    let _, args =
      List.fold params ~init:(inputs, []) ~f:(fun (positional, acc) (p : S.func_param_assign) ->
          match p.name, p.stmt with
          | Some n, S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, S.ExprVariable (_, v))) ->
            let is_in_out = List.Assoc.find inputs n ~equal:String.equal in
            (positional, if Option.value is_in_out ~default:false then (n, v) :: acc else acc)
          | None, S.StmExpr (_, e) -> begin
              match positional, e with
              | (n, true) :: rest, S.ExprVariable (_, v) -> (rest, (n, v) :: acc)
              | _ :: rest, _ -> (rest, acc)
              | [], _ -> ([], acc)
            end
          | _ -> (positional, acc))
    in
    List.rev args

(** Variables written by [stmts]. *)
let writes in_outs instance_types stmts =
  let exprs = AU.get_stmts_exprs stmts in
  let assigned =
    List.filter_map exprs ~f:(function
        | S.ExprBin (_, S.ExprVariable (_, v), (S.ASSIGN | S.ASSIGN_REF), _) ->
          Some { name = S.VarUse.get_name v; ti = S.VarUse.get_ti v; how = Assigned }
        | _ -> None)
  in
  let rec calls = function
    | S.ExprFuncCall (_, (S.StmFuncCall _ as c)) -> [c]
    | S.ExprBin (_, a, _, b) -> calls a @ calls b
    | S.ExprUn (_, _, e) -> calls e
    | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> []
  in
  let stmt_calls =
    List.concat_map stmts ~f:all_stmts
    |> List.filter ~f:(function S.StmFuncCall _ -> true | _ -> false)
  in
  let by_calls =
    (stmt_calls @ List.concat_map exprs ~f:calls)
    |> List.concat_map ~f:(function
        | S.StmFuncCall (_, f, params) ->
          let name = S.Function.get_name f in
          let callee = Option.value (Map.find instance_types name) ~default:name in
          List.filter_map params ~f:(fun (p : S.func_param_assign) ->
              match p.name, p.stmt with
              | Some out, S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) ->
                Some { name = S.VarUse.get_name v; ti = S.VarUse.get_ti v; how = Output (out, name) }
              | _ -> None)
          @ List.map (in_out_args in_outs callee params) ~f:(fun (param, v) ->
              { name = S.VarUse.get_name v; ti = S.VarUse.get_ti v; how = In_out (param, name) })
        | _ -> [])
  in
  assigned @ by_calls
(* }}} *)

let check_for in_outs instance_types (ctrl : S.for_control) body =
  let control =
    match ctrl.assign with
    | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, v), S.ASSIGN, _)) -> [S.VarUse.get_name v]
    | _ -> []
  in
  let bounds =
    reads ctrl.range_end @ reads ctrl.range_step
    |> List.filter ~f:(fun n -> not (List.mem control n ~equal:String.equal))
    |> List.dedup_and_sort ~compare:String.compare
  in
  let body_writes = writes in_outs instance_types body in
  let report protected what =
    List.concat_map protected ~f:(fun p ->
        List.filter_map body_writes ~f:(fun w ->
            if not (overlaps w.name p) then None
            else
              let by = match w.how with
                | Assigned -> ""
                | Output (out, f) -> Printf.sprintf " (by output %s of %s)" out f
                | In_out (param, f) -> Printf.sprintf " (passed to VAR_IN_OUT %s of %s)" param f
              in
              Some (Warn.mk_at w.ti "PLCOPEN-L22" (what w.name by))))
  in
  report control (Printf.sprintf "Loop variable '%s' should not be modified inside a FOR loop%s")
  @ report bounds (Printf.sprintf
                     "Variable '%s' of the final value or increment of a FOR loop should not be \
                      modified inside the loop%s")

let check_elem in_outs elem =
  let instance_types =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  (* [AU.get_pou_stmts] doesn't include the loops themselves. *)
  List.concat_map (AU.get_top_stmts elem) ~f:all_stmts
  |> List.concat_map ~f:(function
      | S.StmFor (_, ctrl, body) -> check_for in_outs instance_types ctrl body
      | _ -> [])

let do_check elems =
  let in_outs = in_outs elems in
  List.concat_map elems ~f:(check_elem in_outs)
  (* A change inside nested loops is reported once for each variable. *)
  |> List.dedup_and_sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      match Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column) with
      | 0 -> String.compare a.msg b.msg
      | c -> c)

let detector : Detector.t = {
  id = "PLCOPEN-L22";
  name = "Loop variables should not be modified inside a FOR loop";
  summary =
    "Modifying the control variable of a [FOR] loop, or its final value or increment, \
     inside the loop body leads to unpredictable iteration behavior.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-L22";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
