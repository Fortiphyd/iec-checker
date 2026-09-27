(** Pointers, references and addresses in a POU, for PLCopen E2 and E3. *)
open IECCheckerCore
module S = Syntax

type t

val of_pou : S.iec_library_element list -> S.iec_library_element -> t
(** [of_pou elements pou] Which variables of [pou] hold pointers or
    addresses: those declared POINTER TO or REF_TO, including globals, and
    integers assigned an address, such as [a := ADR(x)]. *)

val is_address : t -> S.expr -> bool
(** Whether an expression is a pointer, a reference or an address: such a
    variable, REF(x), NULL, ADR(x) or __NEW(...), or arithmetic on one.
    A dereferenced pointer is a value, not an address. *)

val is_arithmetic : S.operator -> bool
