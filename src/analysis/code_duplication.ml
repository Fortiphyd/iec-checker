open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

module StatementMap = Map.Make(Int)



let rec hash_exprs expr =
  match expr with
      | S.ExprBin (_,e1, op, e2) -> begin
         let hash1 = hash_exprs e1 in
         let hash2 = Hashtbl.hash op in
         let hash3 = hash_exprs e2 in
         hash1 lxor hash2 lxor hash3
        end
      | S.ExprUn (_,op,e) -> begin
          let hash1 = Hashtbl.hash op in
          let hash2 = hash_exprs e in
          hash1 lxor hash2
        end
      (*| S.ExprVariable (_, e) | S.ExprConstant (_, e) | S.ExprFuncCall (_, e) -> Hashtbl.hash e*)
      
      | S.ExprVariable (_, e) -> let name = S.VarUse.get_name e in Hashtbl.hash name
      | _ -> 0
        
let rec get_stmt_exprs stmt =

  let get_nested stmts =
    List.fold_left stmts ~init:0 ~f:(fun acc es -> acc lxor (get_stmt_exprs es))
  in
  match stmt with
  | S.StmExpr (_, e) -> hash_exprs e
  | S.StmElsif (_, cond_s, ss) -> (get_nested [cond_s]) lxor (get_nested ss)
  | S.StmIf (_, cond_s, body_ss, elsif_ss, else_ss) -> (
      (get_nested [cond_s]) lxor
      (get_nested body_ss) lxor
      (get_nested elsif_ss) lxor
      (get_nested else_ss)
    )
  | S.StmCase (_, cond_s, case_sels, else_ss) ->
    begin
      let case_stmts =
        List.fold_left
          case_sels
          ~init:0
          ~f:(fun acc case_sel -> acc lxor (get_nested case_sel.case) lxor (get_nested case_sel.body))
      in
      (get_nested [cond_s]) lxor
      case_stmts lxor
      (get_nested else_ss)
    end
  | S.StmFor (_, ctrl, body_stmts) -> (
      (get_nested [ctrl.assign]) lxor
      Hashtbl.hash ctrl.range_end lxor
      Hashtbl.hash ctrl.range_step lxor
      (get_nested body_stmts)
    )
  | S.StmWhile (_, cond_stmt, ss) -> (get_nested [cond_stmt]) lxor (get_nested ss)
  | S.StmRepeat (_, body_stmts, cond_stmt) -> (get_nested body_stmts) lxor (get_nested [cond_stmt])
  | S.StmFuncCall (_, _, func_params) -> begin
      let func_params_stmts = List.fold_left
          func_params
          ~init:[]
          ~f:(fun acc fp -> acc @ [fp.stmt])
      in
      (get_nested func_params_stmts)
    end
  | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> 0

let check_pou elem =
  AU.get_pou_stmts elem
  |> List.fold_left
    ~init:[]
    ~f:(fun acc stmt -> begin 
    let hash = get_stmt_exprs stmt in
  let text = Printf.sprintf "hash: %d" hash in
  let ti = S.stmt_get_ti stmt in
  acc @ [Warn.mk ti.linenr 0 "Code Dupe" text]
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
