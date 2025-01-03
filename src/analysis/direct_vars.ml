open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

let rec check_taint assigned_var rhs tainted_var = 
  let assigned_var_name = S.VarUse.get_name assigned_var in
  let text = Printf.sprintf "Variable %s has been tainted by variable %s" assigned_var_name tainted_var in
  let ti = S.VarUse.get_ti assigned_var in
    match rhs with
    | S.ExprVariable (_, var) -> 
      let varname = S.VarUse.get_name var in
        if String.compare varname tainted_var = 0
        then
         [Warn.mk ti.linenr ti.col "TaintedVariable" text]
        else
         []
    | S.ExprConstant (_,_) -> []
    | S.ExprUn (_, _, e) ->
      check_taint assigned_var e tainted_var
    | S.ExprBin (_, l, _, r) -> 
      let right_result = check_taint assigned_var r tainted_var in
      let left_result = check_taint assigned_var l tainted_var in
      if List.length right_result = 0 && List.length left_result = 0
      then
        []
      else
        [Warn.mk ti.linenr ti.col "TaintedVariable" text]
    | _ -> []

let trace_taint elem tainted_var =
  AU.get_pou_exprs elem
  |> List.fold_left
    ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin (_,(S.ExprVariable (_, assigned_var)),(S.ASSIGN), rhs) -> acc @ check_taint assigned_var rhs tainted_var
          | _ -> acc
        end)

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
          acc @ trace_taint elem var_name
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
