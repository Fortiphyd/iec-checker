(** Types of expressions.

    Types are found from declarations only: variables, array elements, struct
    members and function block outputs, typed literals, conversion functions,
    standard functions and the results of functions. Nothing is propagated
    between statements. *)
open IECCheckerCore
module S = Syntax

type t =
  | Elem of S.elementary_ty
  | Int_literal of int option
  (** Untyped integer literal, and its value if known. It takes the type of
      the context it is used in. *)
  | Real_literal (** Untyped real literal *)
  | Unknown

type env
(** Declarations visible in a POU. *)

val env_of : S.iec_library_element list -> S.iec_library_element -> env
(** [env_of elements pou] Declarations visible in [pou], which is one of
    [elements]. *)

val type_of : env -> S.expr -> t

val var_type : env -> S.VarUse.t -> t

(** {2 Numeric types} *)

(** Signed and unsigned integers with their width in bits, and reals with the
    width of their mantissa. Bit strings are unsigned integers. *)
type num = Signed of int | Unsigned of int | Float of int

val num_of : S.elementary_ty -> num option

val fits : num -> num -> bool
(** [fits src dst] Every value of [src] can be represented exactly in
    [dst]. *)

val to_string : t -> string
