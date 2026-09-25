open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

let get_ty_size = function
  | S.NIL -> 1
  | S.STRING len -> len
  | S.WSTRING len -> len * 2
  | S.CHAR len -> len
  | S.WCHAR len -> len * 2
  | S.TIME -> 8
  | S.LTIME -> 16
  | S.SINT -> 1
  | S.INT -> 2
  | S.DINT -> 4
  | S.LINT -> 8
  | S.USINT -> 1
  | S.UINT -> 2
  | S.UDINT -> 4
  | S.ULINT -> 8
  | S.REAL -> 4
  | S.LREAL -> 8
  | S.DATE -> 8
  | S.LDATE -> 16
  | S.TIME_OF_DAY -> 8
  | S.TOD -> 8
  | S.LTOD -> 16
  | S.DATE_AND_TIME -> 16
  | S.LDATE_AND_TIME -> 16
  | S.DT -> 8
  | S.LDT -> 16
  | S.BOOL -> 1
  | S.BYTE -> 1
  | S.WORD -> 2
  | S.DWORD -> 4
  | S.LWORD -> 8

(** Size of a value of type [ty] in bits. *)
let ty_bits = function
  | S.BOOL -> 1
  | ty -> 8 * get_ty_size ty

(** Size of the address unit of [dir_var] in bits. See 6.5.5.2: no size
    prefix means a single bit. *)
let unit_bits dir_var =
  match S.DirVar.get_size dir_var with
  | Some S.DirVar.SizeB -> 8
  | Some S.DirVar.SizeW -> 16
  | Some S.DirVar.SizeD -> 32
  | Some S.DirVar.SizeL -> 64
  | Some S.DirVar.SizeX | Some S.DirVar.SizeNone | None -> 1

(** Directly represented variables declared in [elem] with the number of
    address units they occupy. *)
let located_vars elem =
  AU.get_var_decls elem
  |> List.filter_map ~f:(fun decl ->
      match S.VarDecl.get_located_at decl with
      | Some dir_var when Option.is_some (S.DirVar.get_loc dir_var)
                       && not (List.is_empty (S.DirVar.get_path dir_var)) ->
        let ty = match S.VarDecl.get_ty_spec decl with
          | Some (S.DTyDeclSingleElement (S.DTySpecElementary ty, _)) -> Some ty
          | _ -> None
        in
        let units = match ty with
          | Some ty -> max 1 ((ty_bits ty + unit_bits dir_var - 1) / unit_bits dir_var)
          | None -> 1
        in
        Some (decl, dir_var, ty, units)
      | _ -> None)

(** Addresses in the same area with the same size prefix and the same path
    except for the last element, as [(prefix, last element)]. Addresses with
    different size prefixes are not compared: their mapping onto each other
    is implementation-dependent. *)
let same_space a b =
  let loc v = Option.value_map (S.DirVar.get_loc v) ~default:"" ~f:S.DirVar.location_to_string in
  let pa = S.DirVar.get_path a and pb = S.DirVar.get_path b in
  String.equal (loc a) (loc b)
  && Int.equal (unit_bits a) (unit_bits b)
  && Int.equal (List.length pa) (List.length pb)
  && List.equal Int.equal (List.drop_last_exn pa) (List.drop_last_exn pb)

let overlaps (a, ua) (b, ub) =
  let start v = List.last_exn (S.DirVar.get_path v) in
  same_space a b && start a < start b + ub && start b < start a + ua

let check_elem elem =
  let vars = located_vars elem in
  List.filter_map vars ~f:(fun (decl, dir_var, ty, units) ->
      Option.bind ty ~f:(fun ty ->
          List.find vars ~f:(fun (other, other_var, _, other_units) ->
              not (phys_equal decl other)
              && overlaps (dir_var, units) (other_var, other_units))
          |> Option.map ~f:(fun (_, overlapped_dir_var, _, _) ->
              let ti = S.VarDecl.get_var_ti decl in
              let msg =
                Printf.sprintf "Address of direct variable %s (size %d) should not overlap with direct variable %s"
                  (S.DirVar.get_name dir_var) (get_ty_size ty)
                  (S.DirVar.get_name overlapped_dir_var)
              in
              Warn.mk ti.linenr ti.col "PLCOPEN-CP4" msg)))

let do_check elems =
  List.fold_left elems ~init:[] ~f:(fun acc elem -> acc @ (check_elem elem))

let detector : Detector.t = {
  id = "PLCOPEN-CP4";
  name = "Direct addressing should not overlap";
  summary =
    "Two directly-addressed variables must not occupy overlapping memory.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP4";
  severity = IECCheckerCore.Warn.High;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
