(** Helpers for the naming convention rules (PLCopen N2, N4, N6, N10). *)
open IECCheckerCore
module S = Syntax

val shown : Tok_info.t -> string -> string
(** [shown ti name] The name as written at [ti], or [name] if it isn't
    known. *)

val has_prefix : prefix:string -> string -> bool
(** [has_prefix ~prefix name] Whether [name] starts with [prefix] followed by
    the start of a word: the end of the name, an upper-case letter, a digit or
    an underscore. A prefix that ends with an underscore may be followed by
    anything. So [xReady] and [x_ready] have the prefix [x], and [xylophone]
    doesn't. *)

val type_kinds : S.derived_ty_decl_spec -> string list
(** Keys of the kind of a type declaration: STRUCT, ENUM, ..., most specific
    first. An enum with a base type is NAMED, then ENUM. *)

val element_kinds : S.iec_library_element -> string list
(** Keys of the kind of a type or POU for its name prefix (PLCopen N10), most
    specific first: ENUM, ..., UDT for types; FUNCTION, FUNCTION_BLOCK,
    PROGRAM, CLASS or INTERFACE for POUs. *)

val strip_prefix : (string * string) list -> string list -> string -> string
(** [strip_prefix prefixes keys name] [name] without the prefix configured for
    the first of [keys] that has one, and an underscore after it, if it has
    that prefix. *)

val var_decls : S.iec_library_element -> S.VarDecl.t list
(** Variables declared by an element, including the global variables of the
    resources of a configuration. *)
