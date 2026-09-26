open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn

(* Report outputs, memory and global variables written by program instances
   that run in different tasks. Such tasks can preempt each other, and the
   value left depends on which one ran last.

   Program instances and their tasks come from the RESOURCE blocks of each
   CONFIGURATION. Writes made by function blocks and functions a program
   calls count as writes of the program. Each configuration is a separate
   PLC, so only instances of one configuration are compared. *)

module PM = Program_model

let shown (r : PM.resolved) =
  match r.address, r.global with
  | Some a, _ when String.is_prefix a ~prefix:"%Q" -> "Physical output " ^ a
  | Some a, _ -> "Memory " ^ a
  | None, Some n -> "Global variable " ^ n
  | None, None -> r.key

let check_configuration effects (c : S.configuration_decl) =
  let by_key = String.Table.create () in
  List.iter (PM.instances c) ~f:(fun inst ->
      let writes = Option.value_map inst.type_name ~default:[] ~f:(fun t -> (effects t).PM.writes) in
      List.iter writes ~f:(fun (w : PM.access) ->
          Option.iter (PM.resolve c inst.resource w.target) ~f:(fun r ->
              Hashtbl.add_multi by_key ~key:r.key ~data:(r, inst, w.ti))));
  Hashtbl.data by_key
  |> List.concat_map ~f:(fun entries ->
      let entries = List.rev entries in
      let tasks =
        List.map entries ~f:(fun (_, (inst : PM.instance), _) -> inst.task)
        |> List.dedup_and_sort ~compare:String.compare
      in
      if List.length tasks < 2 then []
      else begin
        let writers =
          List.map entries ~f:(fun (_, (inst : PM.instance), _) ->
              Printf.sprintf "%s (task %s)" inst.name inst.task)
          |> List.dedup_and_sort ~compare:String.compare
        in
        let r, _, _ = List.hd_exn entries in
        let msg =
          Printf.sprintf "%s is written by programs in different tasks: %s"
            (shown r) (String.concat ~sep:", " writers)
        in
        List.map entries ~f:(fun (_, _, (ti : TI.t)) -> (ti, msg))
      end)

let run elements =
  let writes = PM.effects elements in
  let seen = String.Hash_set.create () in
  List.concat_map elements ~f:(function
      | S.IECConfiguration (_, c) -> check_configuration writes c
      | _ -> [])
  (* A write site reached from several instances is reported once. *)
  |> List.filter ~f:(fun ((ti : TI.t), _) ->
      let key = Printf.sprintf "%d:%d" ti.linenr ti.col in
      if Hash_set.mem seen key then false else (Hash_set.add seen key; true))
  |> List.sort ~compare:(fun ((a : TI.t), _) ((b : TI.t), _) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.col) (b.linenr, b.col))
  |> List.map ~f:(fun ((ti : TI.t), msg) -> Warn.mk_at ti "MultiTaskWrite" msg)
