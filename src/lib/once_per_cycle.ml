open Core
open IECCheckerCore

module S = Syntax
module TI = Tok_info

type event = { key : string; shown : string; ti : TI.t }

type repeat = { event : event; first : TI.t; in_loop : bool }

(** Events seen on the current path, with the first one of each key; [None]
    if the path is unreachable. *)
type state = TI.t String.Map.t option

let compare_pos (a : TI.t) (b : TI.t) =
  Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.col) (b.linenr, b.col)

let join (a : state) (b : state) : state =
  match a, b with
  | None, s | s, None -> s
  | Some m1, Some m2 ->
    Some (Map.merge_skewed m1 m2 ~combine:(fun ~key:_ p q -> if compare_pos p q <= 0 then p else q))

let find_repeated ~events stmts =
  let repeats = String.Table.create () in
  let see m (e : event) =
    begin match Map.find m e.key with
      | Some (first : TI.t) ->
        Hashtbl.set repeats ~key:(Printf.sprintf "%d:%d" e.ti.linenr e.ti.col)
          ~data:{ event = e; first; in_loop = compare_pos first e.ti = 0 }
      | None -> ()
    end;
    Map.update m e.key ~f:(function Some first -> first | None -> e.ti)
  in
  let rec walk (st : state) stmt : state =
    match st with
    | None -> None
    | Some m -> begin
        match stmt with
        | S.StmExpr _ | S.StmFuncCall _ -> Some (List.fold (events stmt) ~init:m ~f:see)
        | S.StmIf (_, _, body, elsifs, els) ->
          let bodies =
            body :: List.filter_map elsifs ~f:(function
                | S.StmElsif (_, _, b) -> Some b
                | _ -> None)
          in
          List.fold bodies ~init:(walk_list st els) ~f:(fun acc b -> join acc (walk_list st b))
        | S.StmCase (_, _, sels, els) ->
          List.fold sels ~init:(walk_list st els)
            ~f:(fun acc (sel : S.case_selection) -> join acc (walk_list st sel.body))
        | S.StmFor (_, ctrl, body) -> loop (walk st ctrl.assign) body
        | S.StmWhile (_, _, body) | S.StmRepeat (_, body, _) -> loop st body
        | S.StmReturn _ -> None
        | S.StmElsif _ | S.StmExit _ | S.StmContinue _ | S.StmEmpty _ -> st
      end
  and walk_list st stmts = List.fold stmts ~init:st ~f:walk
  (* A loop body can run several times per cycle, so it is walked twice. *)
  and loop st body =
    let once = walk_list st body in
    join st (join once (walk_list once body))
  in
  ignore (walk_list (Some String.Map.empty) stmts : state);
  Hashtbl.data repeats
  |> List.sort ~compare:(fun a b -> compare_pos a.event.ti b.event.ti)
