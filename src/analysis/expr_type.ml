open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax

type t =
  | Elem of S.elementary_ty
  | Int_literal of int option
  | Real_literal
  | Unknown

type env = {
  vars : S.derived_ty_decl_spec String.Map.t;
  types : S.derived_ty_decl_spec String.Map.t;
  fbs : S.VarDecl.t list String.Map.t;
  funcs : S.iec_data_type String.Map.t;
}

let decl_specs decls =
  List.filter_map decls ~f:(fun d ->
      Option.map (S.VarDecl.get_ty_spec d) ~f:(fun spec -> (S.VarDecl.get_var_name d, spec)))

let of_alist l = String.Map.of_alist_reduce l ~f:(fun first _ -> first)

let env_of elements elem =
  let globals =
    List.concat_map elements ~f:(function
        | S.IECConfiguration (_, c) ->
          c.variables @ List.concat_map c.resources ~f:(fun (r : S.resource_decl) -> r.variables)
        | _ -> [])
  in
  {
    (* The POU's own declarations take precedence over globals. *)
    vars = Map.merge_skewed (of_alist (decl_specs globals))
        (of_alist (decl_specs (AU.get_var_decls elem)))
        ~combine:(fun ~key:_ _ local -> local);
    types = of_alist (List.filter_map elements ~f:(function
        | S.IECType (_, (name, spec)) -> Some (name, spec)
        | _ -> None));
    fbs = of_alist (List.filter_map elements ~f:(function
        | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id, fb.variables)
        | _ -> None));
    funcs = of_alist (List.filter_map elements ~f:(function
        | S.IECFunction (_, f) -> Some (S.Function.get_name f.id, f.return_ty)
        | _ -> None));
  }

(* {{{ Types of declarations *)
(* Enough to resolve chains of type aliases; also guards against cycles. *)
let max_alias_depth = 16

let rec of_spec env depth = function
  | S.DTyDeclSingleElement (se, _) -> of_single env depth se
  | S.DTyDeclSubrange ((ty, _, _), _) -> Elem ty
  | _ -> Unknown

and of_single env depth = function
  | S.DTySpecElementary ty -> Elem ty
  | S.DTySpecSimple name when depth < max_alias_depth -> begin
      match Map.find env.types name with
      | Some spec -> of_spec env (depth + 1) spec
      | None -> Unknown
    end
  | _ -> Unknown

(** Specification of a type by name, following aliases. *)
let rec named_spec env depth name =
  match Map.find env.types name with
  | Some (S.DTyDeclSingleElement (S.DTySpecSimple n, _)) when depth < max_alias_depth ->
    named_spec env (depth + 1) n
  | spec -> spec

(** Members of the standard function blocks. *)
let std_fb_member ty m =
  let open S in
  let t = match ty, m with
    | ("TON" | "TOF" | "TP"), ("IN" | "Q") -> Some BOOL
    | ("TON" | "TOF" | "TP"), ("PT" | "ET") -> Some TIME
    | ("CTU" | "CTD" | "CTUD"), ("PV" | "CV") -> Some INT
    | ("CTU" | "CTD" | "CTUD"), ("CU" | "CD" | "R" | "LD" | "Q" | "QU" | "QD") -> Some BOOL
    | ("R_TRIG" | "F_TRIG"), ("CLK" | "Q") -> Some BOOL
    | ("SR" | "RS"), ("S1" | "R" | "S" | "R1" | "Q1") -> Some BOOL
    | _ -> None
  in
  Option.map t ~f:(fun t -> DTyDeclSingleElement (DTySpecElementary t, None))

(** Type of the member [m] of a value declared with [spec]. *)
let member_spec env spec m =
  match spec with
  | S.DTyDeclSingleElement (S.DTySpecSimple ty, _) -> begin
      match Map.find env.fbs ty, named_spec env 0 ty with
      | Some decls, _ ->
        List.find decls ~f:(fun d -> String.equal (S.VarDecl.get_var_name d) m)
        |> Option.bind ~f:S.VarDecl.get_ty_spec
      | None, Some (S.DTyDeclStructType (_, elems)) ->
        List.find elems ~f:(fun (e : S.struct_elem_spec) -> String.equal e.struct_elem_name m)
        |> Option.map ~f:(fun (e : S.struct_elem_spec) -> S.DTyDeclSingleElement (e.struct_elem_ty, None))
      | None, _ -> std_fb_member ty m
    end
  | _ -> None
(* }}} *)

(* {{{ Types of expressions *)
let bit_type dv =
  match S.DirVar.get_size dv with
  | Some S.DirVar.SizeB -> Elem S.BYTE
  | Some S.DirVar.SizeW -> Elem S.WORD
  | Some S.DirVar.SizeD -> Elem S.DWORD
  | Some S.DirVar.SizeL -> Elem S.LWORD
  | Some S.DirVar.SizeX | Some S.DirVar.SizeNone -> Elem S.BOOL
  | None -> Unknown

let var_type env v =
  match S.VarUse.get_loc v with
  | S.VarUse.DirVar dv -> bit_type dv
  | S.VarUse.SymVar sv ->
    match String.split (S.VarUse.get_name v) ~on:'.' with
    | [] -> Unknown
    | base :: members ->
      let rec follow spec = function
        | [] -> Some spec
        | m :: _ when String.is_prefix m ~prefix:"%X" -> Some (S.DTyDeclSingleElement (S.DTySpecElementary S.BOOL, None))
        | m :: rest -> Option.bind (member_spec env spec m) ~f:(fun s -> follow s rest)
      in
      match Option.bind (Map.find env.vars base) ~f:(fun s -> follow s members) with
      | None -> Unknown
      | Some (S.DTyDeclArrayType (_, elem_ty, _)) ->
        (* Only an element of an array has a scalar type. *)
        if List.is_empty (S.SymVar.get_array_indexes sv) then Unknown
        else begin
          match elem_ty with
          | S.TyElementary ty -> Elem ty
          | S.TyDerived (S.DTyUseSingleElement se) -> of_single env 0 se
          | _ -> Unknown
        end
      | Some spec -> of_spec env 0 spec

let const_type = function
  | S.CInteger (_, Some ty, _) | S.CReal (_, Some ty, _) | S.CBitString (_, Some ty, _) -> Elem ty
  | S.CInteger (_, None, v) | S.CBitString (_, None, v) -> Int_literal (Some v)
  | S.CReal (_, None, _) -> Real_literal
  | S.CBool _ -> Elem S.BOOL
  | S.CTimeValue _ -> Elem S.TIME
  | S.CString _ | S.CPointer _ | S.CRange _ | S.CEnumValue _ -> Unknown

(** Numeric types: signed and unsigned integers with their width in bits, and
    reals with the width of their mantissa. Bit strings are unsigned. *)
type num = Signed of int | Unsigned of int | Float of int

let num_of = function
  | S.SINT -> Some (Signed 8)
  | S.INT -> Some (Signed 16)
  | S.DINT -> Some (Signed 32)
  | S.LINT -> Some (Signed 64)
  | S.USINT | S.BYTE -> Some (Unsigned 8)
  | S.UINT | S.WORD -> Some (Unsigned 16)
  | S.UDINT | S.DWORD -> Some (Unsigned 32)
  | S.ULINT | S.LWORD -> Some (Unsigned 64)
  | S.REAL -> Some (Float 24)
  | S.LREAL -> Some (Float 53)
  | _ -> None

(** Every value of [src] can be represented exactly in [dst]. *)
let fits src dst =
  match src, dst with
  | Signed a, Signed b | Unsigned a, Unsigned b | Float a, Float b -> a <= b
  | Unsigned a, Signed b -> a < b
  | Signed _, Unsigned _ -> false
  | Signed a, Float m -> a - 1 <= m
  | Unsigned a, Float m -> a <= m
  | Float _, (Signed _ | Unsigned _) -> false

let widens a b =
  match num_of a, num_of b with
  | Some x, Some y -> fits x y
  | _ -> false

(** Type of a binary operation on values of types [a] and [b]. Literals take
    the type of the other operand. *)
let common a b =
  match a, b with
  | Unknown, _ | _, Unknown -> Unknown
  | Int_literal _, Int_literal _ -> Int_literal None
  | (Int_literal _ | Real_literal), (Int_literal _ | Real_literal) -> Real_literal
  | (Int_literal _ | Real_literal), Elem t | Elem t, (Int_literal _ | Real_literal) -> Elem t
  | Elem x, Elem y ->
    if Poly.equal x y then Elem x
    else if widens x y then Elem y
    else if widens y x then Elem x
    else Unknown

let elementary_names =
  let open S in
  [SINT; INT; DINT; LINT; USINT; UINT; UDINT; ULINT; REAL; LREAL; BOOL; BYTE; WORD;
   DWORD; LWORD; TIME; LTIME; DATE; TIME_OF_DAY; TOD; DATE_AND_TIME; DT]
  |> List.map ~f:(fun ty -> (S.ety_to_string ty, ty))

(** Arguments of a call, positional or named, in order. *)
let call_args params =
  List.filter_map params ~f:(fun (p : S.func_param_assign) ->
      match p.name, p.stmt with
      | Some n, S.StmExpr (_, S.ExprBin (_, _, S.ASSIGN, e)) -> Some (Some n, e)
      | None, S.StmExpr (_, e) -> Some (None, e)
      | _ -> None)

let rec type_of env = function
  | S.ExprConstant (_, c) -> const_type c
  | S.ExprVariable (_, v) -> var_type env v
  | S.ExprBin (_, _, (S.GT | S.LT | S.GE | S.LE | S.EQ | S.NEQ), _) -> Elem S.BOOL
  | S.ExprBin (_, l, (S.AND | S.OR | S.XOR), r) -> begin
      match type_of env l, type_of env r with
      | Elem S.BOOL, Elem S.BOOL -> Elem S.BOOL
      | a, b -> common a b
    end
  | S.ExprBin (_, l, _, r) -> common (type_of env l) (type_of env r)
  | S.ExprUn (_, S.NEG, e) -> begin
      match type_of env e with
      | Int_literal v -> Int_literal (Option.map v ~f:Int.neg)
      | t -> t
    end
  | S.ExprUn (_, _, e) -> type_of env e
  | S.ExprFuncCall (_, S.StmFuncCall (_, f, params)) -> call_type env (S.Function.get_name f) params
  | S.ExprFuncCall _ -> Unknown

and call_type env name params =
  let args = call_args params in
  let arg_types = List.map args ~f:(fun (_, e) -> type_of env e) in
  let common_of = function
    | [] -> Unknown
    | t :: ts -> List.fold ts ~init:t ~f:common
  in
  match String.substr_index name ~pattern:"_TO_" with
  | Some i ->
    (* Explicit conversions such as DINT_TO_INT. *)
    let target = String.drop_prefix name (i + 4) in
    Option.value_map (List.Assoc.find elementary_names ~equal:String.equal target)
      ~default:Unknown ~f:(fun ty -> Elem ty)
  | None -> begin
      match name, arg_types with
      | "LIMIT", [_; x; _] -> x
      | ("MIN" | "MAX" | "MOVE" | "ABS" | "NEG" | "SQRT" | "LN" | "LOG" | "EXP"
        | "SIN" | "COS" | "TAN" | "ASIN" | "ACOS" | "ATAN" | "EXPT"), _ :: _ ->
        common_of arg_types
      | ("SEL" | "MUX"), _ :: values -> common_of values
      | _ -> begin
          match Map.find env.funcs name with
          | Some (S.TyElementary ty) -> Elem ty
          | Some (S.TyDerived (S.DTyUseSingleElement se)) -> of_single env 0 se
          | _ -> Unknown
        end
    end
(* }}} *)

let to_string = function
  | Elem ty -> S.ety_to_string ty
  | Int_literal _ -> "integer literal"
  | Real_literal -> "real literal"
  | Unknown -> "unknown"
