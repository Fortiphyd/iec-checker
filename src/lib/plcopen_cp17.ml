open Core

module S = IECCheckerCore.Syntax
module W = IECCheckerCore.Warn

(* Collect (read_vars, written_vars) from a statement.
   A variable is considered "written" if it appears as LHS of an ASSIGN expr, "read" otherwise.
   Variables in array subscripts are read even on the LHS: in [arr[i] := 0], [i] is read and
   [arr] is written (IEC 61131-3: [Subscript : Expression]). *)

let rec collect_expr ~in_lhs (reads, writes) e =
  (* in_lhs: Whether the expression is in the left-hand side of an assignment *)
  match e with
  | S.ExprVariable (_, vu) ->
    (* Accessing a member reads or writes the parameter. *)
    let full = S.VarUse.get_name vu in
    let name = Option.value_map (String.lsplit2 full ~on:'.') ~default:full ~f:fst in
    let acc = if in_lhs then (reads, name :: writes) else (name :: reads, writes) in
    (* Variables in array subscripts are read, even on the left-hand side. *)
    List.fold (S.index_exprs vu) ~init:acc ~f:(collect_expr ~in_lhs:false)
  | S.ExprConstant _ -> (reads, writes)
  | S.ExprBin (_, lhs, op, rhs) -> begin
      match op with
      | S.ASSIGN | S.ASSIGN_REF ->
        let (r, w) = collect_expr ~in_lhs:true (reads, writes) lhs in
        collect_expr ~in_lhs:false (r, w) rhs
      | S.SENDTO ->
        collect_expr ~in_lhs:true (reads, writes) rhs
      | _ ->
        let (r, w) = collect_expr ~in_lhs:false (reads, writes) lhs in
        collect_expr ~in_lhs:false (r, w) rhs
    end
  | S.ExprUn (_, _, e') -> collect_expr ~in_lhs:false (reads, writes) e'
  | S.ExprFuncCall (_, stmt) -> collect_stmt (reads, writes) stmt

and collect_stmt (reads, writes) stmt =
  match stmt with
  | S.StmExpr (_, e) -> collect_expr ~in_lhs:false (reads, writes) e
  | S.StmElsif (_, cond, body) ->
    let acc = collect_stmt (reads, writes) cond in
    List.fold_left body ~init:acc ~f:collect_stmt
  | S.StmIf (_, cond, body, elsifs, els) ->
    let acc = collect_stmt (reads, writes) cond in
    let acc = List.fold_left body ~init:acc ~f:collect_stmt in
    let acc = List.fold_left elsifs ~init:acc ~f:collect_stmt in
    List.fold_left els ~init:acc ~f:collect_stmt
  | S.StmCase (_, cond, selections, els) ->
    let acc = collect_stmt (reads, writes) cond in
    let acc = List.fold_left selections ~init:acc ~f:(fun a (sel : S.case_selection) ->
        let a = List.fold_left sel.case ~init:a ~f:collect_stmt in
        List.fold_left sel.body ~init:a ~f:collect_stmt)
    in
    List.fold_left els ~init:acc ~f:collect_stmt
  | S.StmFor (_, fc, body) ->
    let acc = collect_stmt (reads, writes) fc.assign in
    let acc = collect_expr ~in_lhs:false acc fc.range_end in
    let acc = collect_expr ~in_lhs:false acc fc.range_step in
    List.fold_left body ~init:acc ~f:collect_stmt
  | S.StmWhile (_, cond, body) ->
    let acc = collect_stmt (reads, writes) cond in
    List.fold_left body ~init:acc ~f:collect_stmt
  | S.StmRepeat (_, body, cond) ->
    let acc = List.fold_left body ~init:(reads, writes) ~f:collect_stmt in
    collect_stmt acc cond
  | S.StmEmpty _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ ->
    (reads, writes)
  | S.StmFuncCall (_, _, params) ->
    List.fold_left params ~init:(reads, writes) ~f:collect_func_param_assign

and collect_func_param_assign acc (p : S.func_param_assign) =
  match p.stmt with
  | S.StmExpr (_, S.ExprBin (_, _lhs, op, rhs)) ->
    begin match op with
    | S.ASSIGN | S.ASSIGN_REF ->
      (* Consider input assignment in func call: var := input_var, here rhs is read *)
      collect_expr ~in_lhs:false acc rhs
    | S.SENDTO ->
      (* Consider output assignment in func call: var => output_var, here rhs is written *)
      collect_expr ~in_lhs:true acc rhs
    | _ ->
      (* Abnormal conditions, still treated as a regular statement *)
      collect_stmt acc p.stmt
    end
  | _ ->
    (* Non-standard parameter form, conservatively treated as a regular statement *)
    collect_stmt acc p.stmt


let get_pou_info elem =
  match elem with
  | S.IECFunction (_, fd) ->
    Some ("function " ^ (S.Function.get_name fd.id), fd.variables, fd.statements)
  | S.IECFunctionBlock (_, fb) ->
    Some ("function block " ^ (S.FunctionBlock.get_name fb.id), fb.variables, fb.statements)
  | S.IECProgram (_, pd) ->
    Some ("program " ^ pd.name, pd.variables, pd.statements)
  | _ -> None

type param_kind = PInput | POutput | PInOut

let classify_vardecl vd =
  match S.VarDecl.get_attr vd with
  | Some (S.VarDecl.VarIn _) -> Some PInput
  | Some (S.VarDecl.VarOut _) -> Some POutput
  | Some S.VarDecl.VarInOut -> Some PInOut
  | _ -> begin
      match S.VarDecl.get_direction vd with
      | Some S.VarDecl.Input -> Some PInput
      | Some S.VarDecl.Output -> Some POutput
      | None -> None
    end

let check_pou elem =
  match get_pou_info elem with
  | None -> []
  | Some (pou_desc, vars, stmts) ->
    let (reads, writes) =
      List.fold_left stmts ~init:([], []) ~f:collect_stmt
    in
    let read_set = Set.of_list (module String) reads in
    let write_set = Set.of_list (module String) writes in
    List.fold_left vars ~init:[] ~f:(fun acc vd ->
        match classify_vardecl vd with
        | None -> acc
        | Some kind ->
          let name = S.VarDecl.get_var_name vd in
          let ti = S.VarDecl.get_var_ti vd in
          let is_read = Set.mem read_set name in
          let is_written = Set.mem write_set name in
          let mk_warn msg =
            W.mk_at ti "PLCOPEN-CP17" msg
          in
          begin match kind with
            | PInput ->
              let acc =
                if not is_read then
                  (mk_warn (Printf.sprintf
                              "Input parameter '%s' of %s is never read"
                              name pou_desc)) :: acc
                else acc
              in
              if is_written then
                (mk_warn (Printf.sprintf
                            "Input parameter '%s' of %s should not be written"
                            name pou_desc)) :: acc
              else acc
            | POutput ->
              if not is_written then
                (mk_warn (Printf.sprintf
                            "Output parameter '%s' of %s is never written"
                            name pou_desc)) :: acc
              else acc
            | PInOut ->
              if (not is_read) && (not is_written) then
                (mk_warn (Printf.sprintf
                            "In/out parameter '%s' of %s is neither read nor written"
                            name pou_desc)) :: acc
              else acc
          end)

let do_check elems =
  List.concat_map elems ~f:check_pou

let detector : Detector.t = {
  id = "PLCOPEN-CP17";
  name = "Usage of parameters shall match their declaration mode";
  summary = "Input parameters must be read, output parameters must be written, and in/out parameters must be used.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP17";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}