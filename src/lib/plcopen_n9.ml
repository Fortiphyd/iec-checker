open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn

(* Report names shared by elements of different kinds in the same scope:
   tasks, program instances, programs, function blocks, functions, classes,
   interfaces, variables and user-defined types.

   POUs and types are declared in the global scope and are visible
   everywhere. Variables of a POU are in its scope; tasks, program instances
   and global variables of a configuration are in the configuration's. So a
   local variable can't clash with a task, but a POU can clash with
   anything. Names are compared as the lexer upper-cases them: IEC 61131-3
   names are case-insensitive. *)

type kind =
  | Program | Function_block | Function | Class | Interface | Type
  | Variable | Global_variable | Task | Program_instance

let kind_to_string = function
  | Program -> "PROGRAM"
  | Function_block -> "FUNCTION_BLOCK"
  | Function -> "FUNCTION"
  | Class -> "CLASS"
  | Interface -> "INTERFACE"
  | Type -> "type"
  | Variable -> "variable"
  | Global_variable -> "global variable"
  | Task -> "task"
  | Program_instance -> "program instance"

(** Variables and global variables are one kind of element. *)
let same_kind a b =
  match a, b with
  | (Variable | Global_variable), (Variable | Global_variable) -> true
  | _ -> Poly.equal a b

type scope = Global | Pou of string | Config of string

type occurrence = {
  name : string;
  kind : kind;
  scope : scope;
  ti : TI.t;
}

let overlaps a b =
  match a, b with
  | Global, _ | _, Global -> true
  | Pou x, Pou y | Config x, Config y -> String.equal x y
  | Pou _, Config _ | Config _, Pou _ -> false

let mk name kind scope ti = { name; kind; scope; ti }

let variables scope kind decls =
  List.map decls ~f:(fun d -> mk (S.VarDecl.get_var_name d) kind scope (S.VarDecl.get_var_ti d))

let occurrences elem =
  let pou name ti kind =
    mk name kind Global ti :: variables (Pou name) Variable (AU.get_var_decls elem)
  in
  match elem with
  | S.IECFunction (_, f) -> pou (S.Function.get_name f.id) (S.Function.get_ti f.id) Function
  | S.IECFunctionBlock (_, fb) ->
    pou (S.FunctionBlock.get_name fb.id) (S.FunctionBlock.get_ti fb.id) Function_block
  | S.IECProgram (_, p) -> pou p.name p.name_ti Program
  | S.IECClass (_, c) -> pou c.class_name c.class_ti Class
  | S.IECInterface (_, i) -> [mk i.interface_name Interface Global i.interface_ti]
  | S.IECType (_, ti, (name, _)) -> [mk name Type Global ti]
  | S.IECConfiguration (_, c) ->
    let scope = Config c.name in
    variables scope Global_variable c.variables
    @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) ->
        variables scope Global_variable r.variables
        @ List.map r.tasks ~f:(fun t -> mk (S.Task.get_name t) Task scope (S.Task.get_ti t))
        @ List.map r.programs ~f:(fun pc ->
            mk (S.ProgramConfig.get_name pc) Program_instance scope (S.ProgramConfig.get_ti pc)))

let do_check elems =
  let occs = List.concat_map elems ~f:occurrences in
  let by_name =
    List.map occs ~f:(fun o -> (o.name, o)) |> String.Map.of_alist_multi
  in
  List.filter_map occs ~f:(fun o ->
      let others =
        Map.find_multi by_name o.name
        |> List.filter ~f:(fun x -> overlaps o.scope x.scope && not (same_kind o.kind x.kind))
      in
      if List.is_empty others then None
      else begin
        let others =
          List.sort others ~compare:(fun a b ->
              Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
                (a.ti.linenr, a.ti.col) (b.ti.linenr, b.ti.col))
          |> List.map ~f:(fun x -> Printf.sprintf "a %s on line %d" (kind_to_string x.kind) x.ti.linenr)
          |> List.remove_consecutive_duplicates ~equal:String.equal
        in
        let shown = if String.is_empty o.ti.raw then o.name else o.ti.raw in
        let msg =
          Printf.sprintf "Name %s of this %s is also used for %s" shown
            (kind_to_string o.kind) (String.concat ~sep:", " others)
        in
        Some (Warn.mk_for_name ~name:o.name o.ti.linenr o.ti.col "PLCOPEN-N9" msg)
      end)
  |> List.sort ~compare:(fun (a : Warn.t) (b : Warn.t) ->
      Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare (a.linenr, a.column) (b.linenr, b.column))

let detector : Detector.t = {
  id = "PLCOPEN-N9";
  name = "Different element types should not bear the same name";
  summary =
    "Tasks, programs, function blocks, functions, variables and UDTs should \
     not share a name in the same scope.";
  doc_url = IECCheckerCore.Project.check_doc_url "PLCOPEN-N9";
  severity = IECCheckerCore.Warn.Low;
  plcopen_importance = Some IECCheckerCore.Warn.Medium;
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
