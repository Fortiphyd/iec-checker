open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

module PM = IECCheckerAnalysis.Program_model

(** Names of the global variables declared in configurations and resources. *)
let declared_globals elems =
  List.concat_map elems ~f:(function
      | S.IECConfiguration (_, c) ->
        c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
      | _ -> [])
  |> List.map ~f:S.VarDecl.get_var_name
  |> String.Set.of_list

(** For each PROGRAM, in order, its writes to global variables, including
    those of the function blocks and functions it calls and those of the
    globals its outputs are connected to in configurations. *)
let program_writes elems =
  let effects = PM.effects elems in
  let globals = declared_globals elems in
  let connected =
    List.concat_map elems ~f:(function
        | S.IECConfiguration (_, c) ->
          List.filter_map (PM.instances c) ~f:(fun inst ->
              Option.map inst.type_name ~f:(fun t -> (t, PM.connection_writes inst)))
        | _ -> [])
    |> String.Map.of_alist_reduce ~f:( @ )
  in
  List.filter_map elems ~f:(function
      | S.IECProgram (_, p) ->
        let writes =
          (effects p.name).writes @ Option.value (Map.find connected p.name) ~default:[]
          |> List.filter_map ~f:(fun (w : PM.access) ->
              match w.target with
              | PM.Global (n, is_ext) when is_ext || Set.mem globals n -> Some (n, w)
              | _ -> None)
        in
        Some (p.name, writes)
      | _ -> None)

let do_check elems =
  let writes = program_writes elems in
  let by_global = String.Table.create () in
  List.iter writes ~f:(fun (prog, ws) ->
      List.iter ws ~f:(fun (n, w) -> Hashtbl.add_multi by_global ~key:n ~data:(prog, w)));
  let seen = String.Hash_set.create () in
  Hashtbl.to_alist by_global
  |> List.concat_map ~f:(fun (var_name, entries) ->
      let entries = List.rev entries in
      match entries with
      | [] -> []
      | (first_prog, _) :: _ ->
        (* Flag the writes of every PROGRAM but the first one writing it. *)
        List.filter_map entries ~f:(fun (prog, (w : PM.access)) ->
            let key = Printf.sprintf "%s:%d:%d" var_name w.ti.linenr w.ti.col in
            if String.equal prog first_prog || Hash_set.mem seen key then None
            else begin
              Hash_set.add seen key;
              let through =
                if String.equal w.pou prog then ""
                else Printf.sprintf ", written here for '%s'" prog
              in
              let msg =
                Printf.sprintf
                  "Global variable '%s' should be written by only one PROGRAM \
                   (already written in '%s'%s)"
                  var_name first_prog through
              in
              Some (Warn.mk_at w.ti "PLCOPEN-CP26" msg)
            end))
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-CP26";
  name = "A global variable may be written only by one PROGRAM";
  summary =
    "When multiple PROGRAMs write the same global variable, the resulting \
     value depends on scheduling order, creating a race condition.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP26";
  severity = IECCheckerCore.Warn.High;
  plcopen_importance = Some IECCheckerCore.Warn.Low;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
