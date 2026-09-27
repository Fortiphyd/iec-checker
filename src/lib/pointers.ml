open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

type t = String.Set.t

let is_arithmetic = function
  | S.ADD | S.SUB | S.MUL | S.DIV | S.MOD -> true
  | _ -> false

let address_functions = ["ADR"; "__NEW"]

let rec address vars = function
  | S.ExprVariable (_, v) ->
    (* The whole variable, not an element or member of it *)
    List.is_empty (S.index_exprs v) && Set.mem vars (S.VarUse.get_name v)
  | S.ExprConstant (_, S.CPointer _) -> true
  | S.ExprFuncCall (_, S.StmFuncCall (_, f, _)) ->
    List.mem address_functions (S.Function.get_name f) ~equal:String.equal
  | S.ExprBin (_, a, op, b) when is_arithmetic op -> address vars a || address vars b
  | S.ExprBin _ | S.ExprUn _ | S.ExprConstant _ | S.ExprFuncCall _ -> false

let is_address = address

let of_pou elements elem =
  let globals =
    List.concat_map elements ~f:(function
        | S.IECConfiguration (_, c) ->
          c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
        | _ -> [])
  in
  let declared =
    AU.get_var_decls elem @ globals
    |> List.filter_map ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclRefType _) -> Some (S.VarDecl.get_var_name d)
        | _ -> None)
    |> String.Set.of_list
  in
  let assignments =
    AU.get_pou_exprs elem
    |> List.filter_map ~f:(function
        | S.ExprBin (_, S.ExprVariable (_, v), (S.ASSIGN | S.ASSIGN_REF), rhs)
          when List.is_empty (S.index_exprs v) -> Some (S.VarUse.get_name v, rhs)
        | _ -> None)
  in
  (* Addresses flow from one variable to another. *)
  let rec fix vars =
    let vars' =
      List.fold assignments ~init:vars ~f:(fun acc (n, rhs) ->
          if address acc rhs then Set.add acc n else acc)
    in
    if Set.length vars' = Set.length vars then vars else fix vars'
  in
  fix declared
