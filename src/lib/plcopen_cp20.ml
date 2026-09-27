open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util
module PM = IECCheckerAnalysis.Program_model

(* The rule's exceptions include a counter counting several events in one
   cycle. *)
let multi_call_types = ["CTU"; "CTD"; "CTUD"]

(* A call statement whose name is a declared variable calls a function block
   instance; functions are called by the name of their declaration. Calls
   are counted along each path through the POU body, see {!Once_per_cycle}. *)

(** Declared variables with their type names, if they have one. *)
let decl_types decls =
  List.map decls ~f:(fun d ->
      let ty = match S.VarDecl.get_ty_spec d with
        | Some (S.DTyDeclSingleElement (S.DTySpecSimple ty, _)) -> Some ty
        | _ -> None
      in
      (S.VarDecl.get_var_name d, ty))
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

let events ~effects decls = function
  | S.StmFuncCall (_, f, _) ->
    let name = S.Function.get_name f and ti = S.Function.get_ti f in
    begin match Map.find decls name with
      | None -> []
      | Some (Some ty) when List.mem multi_call_types ty ~equal:String.equal -> []
      | Some ty ->
        (* A function block calls the global instances it uses. *)
        let inner =
          Option.value_map ty ~default:[] ~f:(fun ty ->
              (effects ty).PM.calls
              |> List.filter_map ~f:(fun (c : PM.access) ->
                  match c.target with PM.Global (n, _) -> Some n | PM.Address _ -> None)
              |> List.dedup_and_sort ~compare:String.compare
              |> List.map ~f:(fun n ->
                  Once_per_cycle.{ key = n; shown = Printf.sprintf "%s (called by %s)" n name; ti }))
        in
        Once_per_cycle.{ key = name; shown = name; ti } :: inner
    end
  | _ -> []

let check_elem ~effects elem =
  let decls = decl_types (AU.get_var_decls elem) in
  Once_per_cycle.find_repeated ~events:(events ~effects decls) (AU.get_top_stmts elem)
  |> List.map ~f:(fun (r : Once_per_cycle.repeat) ->
      let where =
        if r.in_loop then "it is called in a loop"
        else Printf.sprintf "it is already called on line %d" r.first.linenr
      in
      let msg =
        Printf.sprintf "Function block instance %s is called more than once per PLC cycle: %s"
          r.event.shown where
      in
      Warn.mk_at r.event.ti "PLCOPEN-CP20" msg)

(** Global instances called by programs that run in the same task. *)
let same_task_calls elements =
  PM.same_task elements ~select:(fun e -> e.PM.calls)
    ~keep:(fun r -> Option.is_some r.PM.global && Option.is_none r.PM.address)
  |> List.map ~f:(fun (c : PM.conflict) ->
      let msg =
        Printf.sprintf
          "Function block instance %s is called more than once per PLC cycle: \
           program %s in the same task (%s) also calls it"
          (Option.value c.resolved.global ~default:c.resolved.key) c.other.name c.instance.task
      in
      Warn.mk_at c.access.ti "PLCOPEN-CP20" msg)

let do_check elements =
  let effects = PM.effects elements in
  let in_pous =
    List.concat_map elements ~f:(function
        | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem ~effects e
        | _ -> [])
  in
  let at (w : Warn.t) = (w.linenr, w.column) in
  let positions = List.map in_pous ~f:at in
  in_pous
  @ List.filter (same_task_calls elements) ~f:(fun w ->
      not (List.mem positions (at w) ~equal:Poly.equal))

let detector : Detector.t = {
  id = "PLCOPEN-CP20";
  name = "Function block instances should be called only once";
  summary =
    "Calling an instance twice per cycle makes timers, counters and edge \
     detectors see inconsistent inputs.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-CP20";
  severity = IECCheckerCore.Warn.High;
  plcopen_importance = Some IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
