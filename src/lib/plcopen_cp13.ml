open Core
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

let pou_name = function
  | S.IECFunction (_, f) -> Some (S.Function.get_name f.id)
  | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id)
  | S.IECProgram (_, p) -> Some p.name
  | _ -> None

(** Calls made by a POU: the name of the called POU and the position of
    each call. Calling a function block instance calls its type. *)
let calls pous elem =
  let instance_types =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let call f =
    let name = S.Function.get_name f in
    let callee = Option.value (Map.find instance_types name) ~default:name in
    Option.some_if (Set.mem pous callee) (callee, S.Function.get_ti f)
  in
  (* Calls in expressions, including in the arguments of other calls, which
     [get_pou_exprs] returns separately. *)
  let rec in_expr = function
    | S.ExprFuncCall (_, S.StmFuncCall (_, f, _)) -> Option.to_list (call f)
    | S.ExprBin (_, a, _, b) -> in_expr a @ in_expr b
    | S.ExprUn (_, _, a) -> in_expr a
    | S.ExprFuncCall _ | S.ExprVariable _ | S.ExprConstant _ -> []
  in
  let statements =
    AU.get_pou_stmts elem
    |> List.filter_map ~f:(function S.StmFuncCall (_, f, _) -> call f | _ -> None)
  in
  statements @ List.concat_map (AU.get_pou_exprs elem) ~f:in_expr
  |> List.dedup_and_sort ~compare:(fun (a, (ta : TI.t)) (b, (tb : TI.t)) ->
      Tuple3.compare ~cmp1:String.compare ~cmp2:Int.compare ~cmp3:Int.compare
        (a, ta.linenr, ta.col) (b, tb.linenr, tb.col))

(** A shortest path of calls from [src] to [dst], without [src]. *)
let path graph src dst =
  let rec bfs visited = function
    | [] -> None
    | (node, rev_path) :: rest ->
      if String.equal node dst then Some (List.rev rev_path)
      else
        let next =
          Option.value (Map.find graph node) ~default:[]
          |> List.map ~f:fst
          |> List.filter ~f:(fun n -> not (Set.mem visited n))
          |> List.dedup_and_sort ~compare:String.compare
        in
        bfs (List.fold next ~init:visited ~f:Set.add)
          (rest @ List.map next ~f:(fun n -> (n, n :: rev_path)))
  in
  bfs (String.Set.singleton src) [(src, [])]

let do_check elems =
  let pous = List.filter_map elems ~f:pou_name |> String.Set.of_list in
  let graph =
    List.filter_map elems ~f:(fun e -> Option.map (pou_name e) ~f:(fun n -> (n, calls pous e)))
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  Map.to_alist graph
  |> List.concat_map ~f:(fun (caller, cs) ->
      List.filter_map cs ~f:(fun (callee, ti) ->
          let through =
            if String.equal callee caller then Some []
            else Option.map (path graph callee caller) ~f:(fun p -> callee :: p)
          in
          Option.map through ~f:(fun through ->
              let how =
                match through with
                | [] -> "directly"
                | ps -> "through " ^ String.concat ~sep:" -> " (List.filter ps ~f:(Fn.non (String.equal caller)))
              in
              let msg =
                Printf.sprintf "POUs shall not call themselves directly or indirectly: %s calls itself %s"
                  caller how
              in
              Warn.mk_at ti "PLCOPEN-CP13" msg)))
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-CP13";
  name = "POUs shall not call themselves directly or indirectly";
  summary = "Recursion is forbidden in IEC 61131-3 — rewrite as a loop.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP13";
  severity = IECCheckerCore.Warn.High;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
