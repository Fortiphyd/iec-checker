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

let get_mccabe_violations elems cfg =
  let cc = CC.eval_mccabe cfg in
  if cc > Config.mccabe_complexity_threshold () then
    let what = Printf.sprintf "%d McCabe complexity" cc in
    match List.find elems ~f:(fun e -> S.get_pou_id e = Cfg.get_pou_id cfg) with
    | Some elem -> [warn_at elem what]
    | None -> [Warn.mk 0 0 "PLCOPEN-CP9" (Printf.sprintf "Code is too complex (%s)" what)]
  else []

let get_statements_num_violations elem =
  let stmts_num = AU.get_stmts_num elem in
  if stmts_num > Config.statements_num_threshold () then
    [warn_at elem (Printf.sprintf "%d statements" stmts_num)]
  else []

let do_check elems cfgs =
  List.fold_left
    cfgs
    ~init:[]
    ~f:(fun acc cfg -> acc @ (get_mccabe_violations elems cfg))
  |> List.append
  @@ List.fold_left
    elems
    ~init:[]
    ~f:(fun acc elem -> acc @ (get_statements_num_violations elem))

let detector : Detector.t = {
  id = "PLCOPEN-CP9";
  name = "Limit the complexity of POU code";
  summary =
    "POUs that exceed McCabe or statement-count thresholds should be split.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP9";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements i.cfgs);
}

