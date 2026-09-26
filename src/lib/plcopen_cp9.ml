open Core
open IECCheckerCore
open IECCheckerAnalysis

module AU = Ast_util
module S = Syntax
module CC = Cyclomatic_complexity

(** A warning about [elem], at its name. *)
let warn_at elem what =
  match S.get_pou_name_as_written elem with
  | Some (name, ti) ->
    Warn.mk_at ti "PLCOPEN-CP9" (Printf.sprintf "%s is too complex (%s)" name what)
  | None -> Warn.mk 0 0 "PLCOPEN-CP9" (Printf.sprintf "Code is too complex (%s)" what)

let get_mccabe_violations elem =
  let cc = CC.mccabe elem in
  if cc > Config.mccabe_complexity_threshold () then
    [warn_at elem (Printf.sprintf "%d McCabe complexity" cc)]
  else []

let get_statements_num_violations elem =
  let stmts_num = AU.get_stmts_num elem in
  if stmts_num > Config.statements_num_threshold () then
    [warn_at elem (Printf.sprintf "%d statements" stmts_num)]
  else []

let do_check elems =
  List.concat_map elems ~f:(function
      | S.IECProgram _ | S.IECFunctionBlock _ | S.IECFunction _ | S.IECClass _ as e ->
        get_mccabe_violations e @ get_statements_num_violations e
      | _ -> [])

let detector : Detector.t = {
  id = "PLCOPEN-CP9";
  name = "Limit the complexity of POU code";
  summary =
    "POUs that exceed McCabe or statement-count thresholds should be split.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP9";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}

