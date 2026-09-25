open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

(* A call statement whose name is a declared variable calls a function block
   instance; functions are called by the name of their declaration. Calls
   are counted along each path through the POU body, see {!Once_per_cycle}. *)

let events instances = function
  | S.StmFuncCall (_, f, _) ->
    let name = S.Function.get_name f in
    if Set.mem instances name
    then [Once_per_cycle.{ key = name; shown = name; ti = S.Function.get_ti f }]
    else []
  | _ -> []

let check_elem elem =
  let instances =
    AU.get_var_decls elem |> List.map ~f:S.VarDecl.get_var_name |> String.Set.of_list
  in
  Once_per_cycle.find_repeated ~events:(events instances) (AU.get_top_stmts elem)
  |> List.map ~f:(fun (r : Once_per_cycle.repeat) ->
      let where =
        if r.in_loop then "it is called in a loop"
        else Printf.sprintf "it is already called on line %d" r.first.linenr
      in
      let msg =
        Printf.sprintf "Function block instance %s is called more than once per PLC cycle: %s"
          r.event.shown where
      in
      Warn.mk r.event.ti.linenr r.event.ti.col "PLCOPEN-CP20" msg)

let do_check elements =
  List.concat_map elements ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ as e -> check_elem e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP20";
  name = "Function block instances should be called only once";
  summary =
    "Calling an instance twice per cycle makes timers, counters and edge \
     detectors see inconsistent inputs.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP20";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
