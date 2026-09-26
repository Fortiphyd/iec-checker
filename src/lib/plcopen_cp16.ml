open Core
open IECCheckerCore

module S = Syntax

(* Report tasks configured to call functions or function blocks rather than
   programs: program configurations whose type is a function, a function
   block declared in the analysed code or a standard function block, and
   function block instances of programs assigned to tasks of their own
   ([PROGRAM P : T(FB1 WITH task)]), as in the rule's example. Types not
   declared in the analysed code may be programs from other files and aren't
   reported. *)

let std_fbs = ["TON"; "TOF"; "TP"; "CTU"; "CTD"; "CTUD"; "R_TRIG"; "F_TRIG"; "SR"; "RS"]

let kinds elems =
  List.filter_map elems ~f:(function
      | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, "FUNCTION_BLOCK")
      | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, "FUNCTION")
      | S.IECProgram (_, p) -> Some (p.name, "PROGRAM")
      | _ -> None)
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)

let check_config kinds (cfg : S.configuration_decl) =
  List.concat_map cfg.resources ~f:(fun (res : S.resource_decl) ->
      List.concat_map res.programs ~f:(fun pc ->
          let task_of = function
            | Some t -> Printf.sprintf "Task %s" (S.Task.get_name t)
            | None -> "The resource"
          in
          let as_type =
            Option.bind (S.ProgramConfig.get_type_name pc) ~f:(fun tn ->
                let kind =
                  match Map.find kinds tn with
                  | Some "PROGRAM" -> None
                  | Some k -> Some k
                  | None when List.mem std_fbs tn ~equal:String.equal -> Some "FUNCTION_BLOCK"
                  | None -> None
                in
                Option.map kind ~f:(fun kind ->
                    Warn.mk_at (S.ProgramConfig.get_ti pc) "PLCOPEN-CP16"
                      (Printf.sprintf "%s should call PROGRAM, not %s '%s'"
                         (task_of (S.ProgramConfig.get_task pc)) kind tn)))
          in
          let fb_tasks =
            List.map (S.ProgramConfig.get_fb_tasks pc) ~f:(fun (t : S.ProgramConfig.fb_task) ->
                Warn.mk_at t.fb_ti "PLCOPEN-CP16"
                  (Printf.sprintf
                     "%s should call PROGRAM, not FUNCTION_BLOCK instance '%s' of program %s"
                     (task_of (Some t.fb_task)) t.fb_name (S.ProgramConfig.get_name pc)))
          in
          Option.to_list as_type @ fb_tasks))

let do_check elems =
  let kinds = kinds elems in
  List.concat_map elems ~f:(function
      | S.IECConfiguration (_, cfg) -> check_config kinds cfg
      | _ -> [])
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-CP16";
  name = "Tasks shall only call program POUs and not function blocks";
  summary =
    "A task in a RESOURCE block should only execute PROGRAM instances, \
     not functions or FUNCTION_BLOCK instances.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP16";
  severity = IECCheckerCore.Warn.Medium;
  plcopen_importance = Some IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
