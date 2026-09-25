open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

(* A physical output is written by assigning to its direct address, to a
   variable located at it, or through an output parameter ([OUT => %QW1]).
   Writes are counted along each path through the POU body: writes in
   exclusive branches are fine, writes in sequence or in a loop body are not. *)

let is_output dv =
  match S.DirVar.get_loc dv with Some S.DirVar.LocQ -> true | _ -> false

let located_outputs decls =
  List.filter_map decls ~f:(fun d ->
      match S.VarDecl.get_located_at d with
      | Some dv when is_output dv -> Some (S.VarDecl.get_var_name d, S.DirVar.get_name dv)
      | _ -> None)
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

(** Output variables declared in configurations, resources and VAR_GLOBAL
    blocks, by name. *)
let global_outputs elements =
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) ->
        c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
      | e ->
        List.filter (AU.get_var_decls e) ~f:(fun d ->
            match S.VarDecl.get_attr d with Some (S.VarDecl.VarGlobal _) -> true | _ -> false))
  |> located_outputs

(** Output variables visible in a POU: its own, and globals it doesn't shadow
    with a variable of the same name. *)
let visible_outputs globals decls =
  let shadowed =
    List.filter_map decls ~f:(fun d ->
        match S.VarDecl.get_attr d with
        | Some (S.VarDecl.VarExternal _) -> None
        | _ -> Some (S.VarDecl.get_var_name d))
    |> String.Set.of_list
  in
  Map.merge_skewed
    (Map.filter_keys globals ~f:(fun n -> not (Set.mem shadowed n)))
    (located_outputs decls)
    ~combine:(fun ~key:_ _ local -> local)

(** The physical output written by assigning to [v], as a key identifying the
    output and a name to show. Elements with non-constant indexes are
    skipped, since which output they write isn't known. *)
let written_output outputs v =
  match S.VarUse.get_loc v with
  | S.VarUse.DirVar dv when is_output dv ->
    let addr = S.DirVar.get_name dv in
    Some (addr, addr)
  | S.VarUse.DirVar _ -> None
  | S.VarUse.SymVar sv ->
    let full = S.VarUse.get_name v in
    let base, member =
      match String.lsplit2 full ~on:'.' with
      | Some (b, m) -> b, "." ^ m
      | None -> full, ""
    in
    let indexes = S.SymVar.get_array_indexes sv in
    Option.bind (Map.find outputs base) ~f:(fun addr ->
        if List.exists indexes ~f:Option.is_none then None
        else
          let idx =
            if List.is_empty indexes then ""
            else Printf.sprintf "[%s]"
                (String.concat ~sep:", "
                   (List.filter_map indexes ~f:(Option.map ~f:Int.to_string)))
          in
          Some (addr ^ member ^ idx, Printf.sprintf "%s%s%s (%s)" base member idx addr))

type ctx = {
  outputs : string String.Map.t;
  warns : Warn.t String.Table.t; (** Keyed by position *)
}

(** Outputs written on the current path, with the position of the first
    write; [None] if the path is unreachable. *)
type state = (int * int) String.Map.t option

let compare_pos = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare

let join (a : state) (b : state) : state =
  match a, b with
  | None, s | s, None -> s
  | Some m1, Some m2 ->
    Some (Map.merge_skewed m1 m2 ~combine:(fun ~key:_ p q -> if compare_pos p q <= 0 then p else q))

let write ctx m v =
  match written_output ctx.outputs v with
  | None -> m
  | Some (key, shown) ->
    let ti = S.VarUse.get_ti v in
    begin match Map.find m key with
      | Some (line, col) ->
        let where =
          if line = ti.linenr && col = ti.col then "it is written in a loop"
          else Printf.sprintf "it is already written on line %d" line
        in
        let msg =
          Printf.sprintf "Physical output %s is written more than once per PLC cycle: %s"
            shown where
        in
        Hashtbl.set ctx.warns ~key:(Printf.sprintf "%d:%d" ti.linenr ti.col)
          ~data:(Warn.mk ti.linenr ti.col "PLCOPEN-CP12" msg)
      | None -> ()
    end;
    Map.update m key ~f:(function Some first -> first | None -> (ti.linenr, ti.col))

let rec walk ctx (st : state) stmt : state =
  match st with
  | None -> None
  | Some m -> begin
      match stmt with
      | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, lhs), (S.ASSIGN | S.ASSIGN_REF), _)) ->
        Some (write ctx m lhs)
      | S.StmIf (_, _, body, elsifs, els) ->
        let bodies =
          body :: List.filter_map elsifs ~f:(function
              | S.StmElsif (_, _, b) -> Some b
              | _ -> None)
        in
        List.fold bodies ~init:(walk_list ctx st els) ~f:(fun acc b -> join acc (walk_list ctx st b))
      | S.StmCase (_, _, sels, els) ->
        List.fold sels ~init:(walk_list ctx st els)
          ~f:(fun acc (sel : S.case_selection) -> join acc (walk_list ctx st sel.body))
      | S.StmFor (_, ctrl, body) -> loop ctx (walk ctx st ctrl.assign) body
      | S.StmWhile (_, _, body) | S.StmRepeat (_, body, _) -> loop ctx st body
      | S.StmFuncCall (_, _, params) ->
        Some (List.fold params ~init:m ~f:(fun m (p : S.func_param_assign) ->
            match p.stmt with
            | S.StmExpr (_, S.ExprBin (_, _, S.SENDTO, S.ExprVariable (_, v))) -> write ctx m v
            | _ -> m))
      | S.StmReturn _ -> None
      | S.StmExpr _ | S.StmElsif _ | S.StmExit _ | S.StmContinue _ | S.StmEmpty _ -> st
    end

and walk_list ctx st stmts = List.fold stmts ~init:st ~f:(walk ctx)

(** A loop body can run several times per cycle, so it is walked twice. *)
and loop ctx st body =
  let once = walk_list ctx st body in
  join st (join once (walk_list ctx once body))

let check_elem globals elem =
  let ctx = {
    outputs = visible_outputs globals (AU.get_var_decls elem);
    warns = String.Table.create ();
  } in
  ignore (walk_list ctx (Some String.Map.empty) (AU.get_top_stmts elem) : state);
  Hashtbl.data ctx.warns
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      compare_pos (a.linenr, a.column) (b.linenr, b.column))

let do_check elements =
  let globals = global_outputs elements in
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem globals e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP12";
  name = "Physical outputs shall be written once per PLC cycle";
  summary =
    "Writing the same output twice in one cycle makes its value depend on \
     statement order.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP12";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
