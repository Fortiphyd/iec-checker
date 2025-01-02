open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

let check_assign_expr (ti : AU.TI.t) lhs =
  let name = S.VarUse.get_name lhs in
    Some(Warn.mk ti.linenr ti.col "Assignment" name)

let check_pou elem =
  AU.get_pou_exprs elem
  |> List.fold_left
    ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin (ti,(S.ExprVariable (_, lhs)),(S.ASSIGN),_) -> begin
              check_assign_expr ti lhs
              |> Caml.Option.fold ~none:[] ~some:(fun w -> [w])
              |> List.append acc
            end
          | _ -> acc
        end)
  
let run elements =
  List.fold_left
    elements
    ~f:(fun warns e ->
        let ws = match e with
          | S.IECProgram _ | S.IECFunction _ | S.IECFunctionBlock _ -> check_pou e
          | _ -> []
        in
        warns @ ws)
    ~init:[]
