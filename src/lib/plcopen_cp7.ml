open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(* Report calls whose error information is never tested: outputs of function
   block instances and functions named like error information (Error,
   ErrorID, xError, iErrorCode, ...) none of which the calling POU reads.
   Reading one of them, such as the Error flag, is enough.

   An error output is tested when the POU reads it (inst.Error), or when it
   is connected with => to a variable that the POU reads or passes on as its
   own output, in-out or global. The outputs of an instance keep their values
   between calls, so they are checked once per instance; those of a function
   at each call. The outputs of function blocks and
   functions declared in the analysed code are known, and PLCopen Motion
   Control blocks (MC_...) have Error and ErrorID. Other library blocks
   aren't checked. *)

(** Whether an output named [name], as written, holds error information. A
    Hungarian prefix such as x in xError is ignored. *)
let is_error_name name =
  let without_prefix =
    let n = String.length name in
    let i = ref 0 in
    while !i < n && Char.is_lowercase name.[!i] do incr i done;
    if !i > 0 && !i < n && Char.is_uppercase name.[!i] then String.drop_prefix name !i else name
  in
  let key = String.uppercase without_prefix |> String.filter ~f:(fun c -> not (Char.equal c '_')) in
  List.mem ["ERR"; "ERROR"; "ERRORID"; "ERRID"; "ERRORCODE"; "ERRCODE"] key ~equal:String.equal

let shown d =
  let ti = S.VarDecl.get_var_ti d in
  if String.is_empty ti.raw then S.VarDecl.get_var_name d else ti.raw

(** Error outputs of each POU declared in [elements], by name. *)
let declared_error_outputs elements =
  List.filter_map elements ~f:(function
      | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, fb.variables)
      | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, f.variables)
      | _ -> None)
  |> List.map ~f:(fun (name, decls) ->
      let outs =
        List.filter decls ~f:(fun d ->
            match S.VarDecl.get_attr d with
            | Some (S.VarDecl.VarOut _) -> is_error_name (shown d)
            | _ -> false)
        (* In declaration order; the parser keeps them in reverse. *)
        |> List.sort ~compare:(fun a b ->
            let ta = S.VarDecl.get_var_ti a and tb = S.VarDecl.get_var_ti b in
            Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (ta.linenr, ta.col) (tb.linenr, tb.col))
        |> List.map ~f:(fun d -> (S.VarDecl.get_var_name d, shown d))
      in
      (name, outs))
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

let error_outputs declared ty =
  match Map.find declared ty with
  | Some outs -> outs
  | None when String.is_prefix ty ~prefix:"MC_" -> [("ERROR", "Error"); ("ERRORID", "ErrorID")]
  | None -> []

(** Names read by the expressions of [elem]: not the targets of assignments
    or of outputs. *)
let reads elem =
  let rec vars = function
    | S.ExprVariable (_, v) -> S.VarUse.get_name v :: List.concat_map (S.index_exprs v) ~f:vars
    | S.ExprBin (_, a, _, b) -> vars a @ vars b
    | S.ExprUn (_, _, a) -> vars a
    | S.ExprConstant _ | S.ExprFuncCall _ -> []
  in
  AU.get_pou_exprs elem
  |> List.concat_map ~f:(function
      | S.ExprBin (_, S.ExprVariable (_, lhs), (S.ASSIGN | S.ASSIGN_REF), rhs) ->
        List.concat_map (S.index_exprs lhs) ~f:vars @ vars rhs
      | S.ExprBin (_, _, S.SENDTO, _) -> []
      | e -> vars e)
  |> String.Set.of_list

let base name = Option.value_map (String.lsplit2 name ~on:'.') ~default:name ~f:fst

let check_elem elements declared elem =
  let decls = AU.get_var_decls elem in
  let instances =
    List.filter_map decls ~f:(fun d ->
        match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some (S.VarDecl.get_var_name d, ty)
        | _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  (* Variables whose value leaves the POU *)
  let passed_on =
    List.filter_map decls ~f:(fun d ->
        match S.VarDecl.get_attr d with
        | Some (S.VarDecl.VarOut _ | S.VarDecl.VarInOut | S.VarDecl.VarExternal _
               | S.VarDecl.VarGlobal _) -> Some (S.VarDecl.get_var_name d)
        | _ -> None)
    |> String.Set.of_list
  in
  let read = reads elem in
  let used name = Set.mem read name || Set.mem passed_on (base name) in
  let rec calls_in = function
    | S.ExprFuncCall (_, (S.StmFuncCall _ as c)) -> [c]
    | S.ExprBin (_, a, _, b) -> calls_in a @ calls_in b
    | S.ExprUn (_, _, a) -> calls_in a
    | S.ExprFuncCall _ | S.ExprVariable _ | S.ExprConstant _ -> []
  in
  let calls =
    List.filter (AU.get_pou_stmts elem) ~f:(function S.StmFuncCall _ -> true | _ -> false)
    @ List.concat_map (AU.get_pou_exprs elem) ~f:calls_in
    |> List.filter_map ~f:(function
        | S.StmFuncCall (_, f, params) -> Some (f, params)
        | _ -> None)
    (* Calls in expressions are statements of their own too. *)
    |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) ->
        let ta = S.Function.get_ti a and tb = S.Function.get_ti b in
        Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (ta.linenr, ta.col) (tb.linenr, tb.col))
  in
  (* The calls of each instance together; each call of a function alone *)
  let by_callee =
    List.filter_map calls ~f:(fun (f, params) ->
        let name = S.Function.get_name f in
        match Map.find instances name with
        | Some ty -> Some (Either.First ((name, ty), (f, params)))
        | None when Map.mem declared name -> Some (Either.Second ((name, name, false), [(f, params)]))
        | None -> None)
    |> List.partition_map ~f:Fn.id
    |> fun (inst_calls, func_calls) ->
    (List.Assoc.sort_and_group inst_calls ~compare:(fun (a, _) (b, _) -> String.compare a b)
     |> List.map ~f:(fun ((name, ty), sites) -> ((name, ty, true), sites)))
    @ func_calls
  in
  let type_name ty =
    List.find_map elements ~f:(fun e ->
        match S.get_pou_name_as_written e with
        | Some (n, _) when String.equal (String.uppercase n) ty -> Some n
        | _ -> None)
    |> Option.value ~default:ty
  in
  List.filter_map by_callee ~f:(fun ((name, ty, is_instance), sites) ->
      let connected out =
        List.concat_map sites ~f:(fun (_, params) ->
            List.filter_map params ~f:(fun (p : S.func_param_assign) ->
                match p.name, p.stmt with
                | Some n, S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v)))
                  when String.equal n out -> Some (S.VarUse.get_name v)
                | _ -> None))
      in
      let outs = error_outputs declared ty in
      let tested (out, _) =
        (is_instance && used (name ^ "." ^ out)) || List.exists (connected out) ~f:used
      in
      match outs, sites with
      | [], _ | _, [] -> None
      | _ when List.exists outs ~f:tested -> None
      | outs, (f, _) :: _ ->
        let ti = S.Function.get_ti f in
        let callee = if String.is_empty ti.raw then name else ti.raw in
        let outs = String.concat ~sep:", " (List.map outs ~f:snd) in
        let what =
          if is_instance then Printf.sprintf "%s (%s)" callee (type_name ty) else callee
        in
        Some (Warn.mk_at ti "PLCOPEN-CP7"
                (Printf.sprintf "Error information shall be tested: %s of %s %s never read"
                   outs what (if List.length (String.split outs ~on:',') > 1 then "are" else "is"))))

let do_check elements =
  let declared = declared_error_outputs elements in
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        check_elem elements declared e
      | _ -> [])
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-CP7";
  name = "Error information shall be tested";
  summary = "Error outputs of function block and function calls should be read.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP7";
  severity = Warn.Medium;
  plcopen_importance = Some Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
