open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util
module T = IECCheckerAnalysis.Expr_type

(* Report implicit conversions that may lose value or precision: in
   assignments, in the arguments of calls, and between the operands of an
   operation. The rule allows implicit conversions without loss (IEC 61131-3,
   Table 11), such as INT to DINT or INT to REAL. *)

let int_range = function
  | T.Signed b when b < 63 -> Some (-(1 lsl (b - 1)), (1 lsl (b - 1)) - 1)
  | T.Unsigned b when b < 63 -> Some (0, (1 lsl b) - 1)
  | T.Signed _ | T.Unsigned _ | T.Float _ -> None

let is_int = function T.Signed _ | T.Unsigned _ -> true | T.Float _ -> false

let warn (ti : Tok_info.t) msg = Warn.mk_at ti "PLCOPEN-CP25" msg

(** A warning if storing [value] in [what], of type [dst], converts it
    implicitly with a possible loss. *)
let check_conversion env ~what ti value dst =
  match dst with
  | T.Elem dst_ty -> begin
      match T.num_of dst_ty, T.type_of env value with
      | Some dst, T.Elem src_ty -> begin
          match T.num_of src_ty with
          | Some src when not (T.fits src dst) ->
            Some (warn ti (Printf.sprintf
                             "Implicit conversion from %s to %s %s may lose information; \
                              convert it explicitly"
                             (S.ety_to_string src_ty) (S.ety_to_string dst_ty) what))
          | _ -> None
        end
      | Some dst, T.Int_literal (Some v) -> begin
          match int_range dst with
          | Some (lo, hi) when v < lo || v > hi ->
            Some (warn ti (Printf.sprintf "Value %d is out of range for %s %s (%d..%d)"
                             v (S.ety_to_string dst_ty) what lo hi))
          | _ -> None
        end
      | Some dst, T.Real_literal when is_int dst ->
        Some (warn ti (Printf.sprintf
                         "Implicit conversion of a real literal to %s %s loses its fraction"
                         (S.ety_to_string dst_ty) what))
      | _ -> None
    end
  | _ -> None

(* {{{ Assignments *)
(** Assignments of the statements, without the arguments of calls. *)
let rec assignments = function
  | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, lhs), S.ASSIGN, rhs)) -> [(lhs, rhs)]
  | S.StmIf (_, _, body, elsifs, els) ->
    List.concat_map (body @ elsifs @ els) ~f:assignments
  | S.StmElsif (_, _, body) | S.StmWhile (_, _, body) | S.StmRepeat (_, body, _) ->
    List.concat_map body ~f:assignments
  | S.StmCase (_, _, sels, els) ->
    List.concat_map sels ~f:(fun (sel : S.case_selection) -> List.concat_map sel.body ~f:assignments)
    @ List.concat_map els ~f:assignments
  | S.StmFor (_, ctrl, body) -> assignments ctrl.assign @ List.concat_map body ~f:assignments
  | _ -> []

let check_assignments env elem =
  List.concat_map (AU.get_top_stmts elem) ~f:assignments
  |> List.filter_map ~f:(fun (lhs, rhs) ->
      check_conversion env ~what:("variable " ^ S.VarUse.get_name lhs)
        (S.VarUse.get_ti lhs) rhs (T.var_type env lhs))
(* }}} *)

(* {{{ Arguments *)
(** Inputs of a function or function block, in declaration order. *)
let inputs decls =
  List.filter decls ~f:(fun d ->
      match S.VarDecl.get_attr d with
      | Some (S.VarDecl.VarIn _ | S.VarDecl.VarInOut) -> true
      | _ -> false)
  |> List.sort ~compare:(fun a b ->
      let ta = S.VarDecl.get_var_ti a and tb = S.VarDecl.get_var_ti b in
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (ta.linenr, ta.col) (tb.linenr, tb.col))

let check_arguments elements env elem =
  let callees =
    List.filter_map elements ~f:(function
        | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, fb.variables)
        | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, f.variables)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let instance_types =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  AU.get_pou_stmts elem
  |> List.concat_map ~f:(function
      | S.StmFuncCall (_, f, params) ->
        let name = S.Function.get_name f in
        let callee = Option.value (Map.find instance_types name) ~default:name in
        begin match Map.find callees callee with
          | None -> []
          | Some decls ->
            let inputs = inputs decls in
            let _, args =
              List.fold params ~init:(inputs, []) ~f:(fun (positional, acc) (p : S.func_param_assign) ->
                  match p.name, p.stmt with
                  | Some n, S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) ->
                    let decl = List.find inputs ~f:(fun d -> String.equal (S.VarDecl.get_var_name d) n) in
                    (positional, (decl, e) :: acc)
                  | None, S.StmExpr (_, e) -> begin
                      match positional with
                      | d :: rest -> (rest, (Some d, e) :: acc)
                      | [] -> ([], acc)
                    end
                  | _ -> (positional, acc))
            in
            List.filter_map (List.rev args) ~f:(fun (decl, e) ->
                Option.bind decl ~f:(fun d ->
                    Option.bind (S.VarDecl.get_ty_spec d) ~f:(fun spec ->
                        check_conversion env
                          ~what:(Printf.sprintf "parameter %s of %s" (S.VarDecl.get_var_name d) callee)
                          (S.expr_get_ti e) e (T.type_of_spec env spec))))
        end
      | _ -> [])
(* }}} *)

(* {{{ Operands *)
let converting = function
  | S.ADD | S.SUB | S.MUL | S.DIV | S.MOD | S.POW
  | S.GT | S.LT | S.GE | S.LE | S.EQ | S.NEQ -> true
  | _ -> false

(** Operations on two numeric types neither of which converts to the other
    without loss, e.g. DINT and REAL, or INT and UINT. *)
let check_operands env elem =
  (* Call arguments are separate expressions in [get_pou_exprs]. *)
  let rec check acc = function
    | S.ExprBin (ti, l, op, r) ->
      let acc = check (check acc l) r in
      begin match converting op, T.type_of env l, T.type_of env r with
        | true, T.Elem a, T.Elem b -> begin
            match T.num_of a, T.num_of b with
            | Some x, Some y when not (T.fits x y) && not (T.fits y x) ->
              warn ti (Printf.sprintf
                         "Implicit conversion between %s and %s may lose value or precision; \
                          convert one operand explicitly"
                         (S.ety_to_string a) (S.ety_to_string b)) :: acc
            | _ -> acc
          end
        | _ -> acc
      end
    | S.ExprUn (_, _, e) -> check acc e
    | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> acc
  in
  AU.get_pou_exprs elem |> List.fold ~init:[] ~f:check |> List.rev
(* }}} *)

let check_elem elements elem =
  let env = T.env_of elements elem in
  check_assignments env elem @ check_arguments elements env elem @ check_operands env elem

let do_check elements =
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem elements e
      | _ -> [])
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-CP25";
  name = "Data type conversion should be explicit";
  summary =
    "Implicit conversions that may lose value or precision should be explicit.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP25";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
