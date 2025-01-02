open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

let get_assigned_vars elem =
  AU.get_pou_exprs elem
  |> List.fold_left
    ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin (_,(S.ExprVariable (_, lhs)),(S.ASSIGN),_) -> acc @ [S.VarUse.get_name lhs]
          | _ -> acc
        end)

let get_located_vars_decls elem =
  AU.get_var_decls elem
  |> List.fold_left
    ~init:[]
    ~f:(fun acc var_decl -> begin
          match (S.VarDecl.get_located_at var_decl) with
          | Some _ -> acc @ [S.VarDecl.get_var_name var_decl]
          | None -> acc
        end)
        
let check_pou elem = 
  let module StringSet = Set.Make(String) in
  let decls = get_located_vars_decls elem in 
  let decl_set = StringSet.of_list (decls) in
  let assigns = get_assigned_vars elem in
  let assign_set = StringSet.of_list (assigns) in
  StringSet.diff decl_set assign_set
  |> Set.fold ~init:[]
    ~f:(fun acc var_name -> begin
          let ti = AU.get_ti_by_name_exn elem var_name in
          let text = Printf.sprintf "Found tainted variable: %s" var_name in
          acc @ [Warn.mk ti.linenr ti.col "TaintedVariable" text]
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
