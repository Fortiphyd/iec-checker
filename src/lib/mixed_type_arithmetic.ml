open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util
module T = IECCheckerAnalysis.Expr_type

(* Report arithmetic on operands of different numeric types, such as INT and
   REAL, or an integer and a real literal. Untyped integer literals take the
   type of the other operand and aren't reported. *)

let arith = function
  | S.ADD | S.SUB | S.MUL | S.DIV | S.MOD | S.POW -> true
  | _ -> false

let is_int ty =
  match T.num_of ty with
  | Some (T.Signed _ | T.Unsigned _) -> true
  | _ -> false

let mixed l r =
  match l, r with
  | T.Elem a, T.Elem b ->
    Option.is_some (T.num_of a) && Option.is_some (T.num_of b) && not (Poly.equal a b)
  | T.Elem a, T.Real_literal | T.Real_literal, T.Elem a -> is_int a
  | _ -> false

let check_elem elements elem =
  let env = T.env_of elements elem in
  (* Call arguments are separate expressions in [get_pou_exprs]. *)
  let rec check acc = function
    | S.ExprBin (ti, l, op, r) ->
      let acc = check (check acc l) r in
      let tl = T.type_of env l and tr = T.type_of env r in
      if arith op && mixed tl tr then
        let msg =
          Printf.sprintf "Arithmetic on %s and %s; convert one operand explicitly"
            (T.to_string tl) (T.to_string tr)
        in
        Warn.mk ti.linenr ti.col "MixedTypeArithmetic" msg :: acc
      else acc
    | S.ExprUn (_, _, e) -> check acc e
    | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> acc
  in
  AU.get_pou_exprs elem |> List.fold ~init:[] ~f:check |> List.rev

let do_check elements =
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem elements e
      | _ -> [])

let detector : Detector.t = {
  id = "MixedTypeArithmetic";
  name = "Arithmetic operands should have the same type";
  summary =
    "Mixing numeric types, e.g. INT and REAL, relies on implicit conversions.";
  doc_url = "";
  severity = Warn.Low;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
