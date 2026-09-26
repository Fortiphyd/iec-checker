open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util
module T = IECCheckerAnalysis.Expr_type

(* Report assignments of values that may not fit in the assigned variable:
   a wider or differently signed integer, a real to an integer or LREAL to
   REAL, or a literal out of range. An explicit conversion function states
   the intent and isn't reported. *)

let int_range = function
  | T.Signed b when b < 63 -> Some (-(1 lsl (b - 1)), (1 lsl (b - 1)) - 1)
  | T.Unsigned b when b < 63 -> Some (0, (1 lsl b) - 1)
  | T.Signed _ | T.Unsigned _ | T.Float _ -> None

let is_int = function T.Signed _ | T.Unsigned _ -> true | T.Float _ -> false

let check_assign env lhs rhs =
  let name = S.VarUse.get_name lhs in
  let ti = S.VarUse.get_ti lhs in
  let warn msg = Some (Warn.mk_at ti "NarrowingAssignment" msg) in
  match T.var_type env lhs with
  | T.Elem dst_ty -> begin
      match T.num_of dst_ty, T.type_of env rhs with
      | Some dst, T.Elem src_ty -> begin
          match T.num_of src_ty with
          (* PLCOPEN-CP25 reports a variable of an integer type assigned to a
             real one and the other way around. *)
          | Some src when (match rhs with S.ExprVariable _ -> true | _ -> false)
                       && not (Bool.equal (is_int src) (is_int dst)) -> None
          | Some src when not (T.fits src dst) ->
            warn (Printf.sprintf
                    "%s value assigned to %s variable %s may not fit; convert it explicitly"
                    (S.ety_to_string src_ty) (S.ety_to_string dst_ty) name)
          | _ -> None
        end
      | Some dst, T.Int_literal (Some v) -> begin
          match int_range dst with
          | Some (lo, hi) when v < lo || v > hi ->
            warn (Printf.sprintf "Value %d is out of range for %s variable %s (%d..%d)"
                    v (S.ety_to_string dst_ty) name lo hi)
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None

let check_elem elements elem =
  let env = T.env_of elements elem in
  AU.get_pou_exprs elem
  |> List.filter_map ~f:(function
      | S.ExprBin (_, S.ExprVariable (_, lhs), S.ASSIGN, rhs) -> check_assign env lhs rhs
      | _ -> None)

let do_check elements =
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem elements e
      | _ -> [])

let detector : Detector.t = {
  id = "NarrowingAssignment";
  name = "Assigned values should fit in the assigned variable";
  summary =
    "Implicitly narrowing a value, e.g. DINT to INT or REAL to INT, can \
     silently change it.";
  doc_url = "";
  severity = Warn.Medium;
  plcopen_importance = None;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
