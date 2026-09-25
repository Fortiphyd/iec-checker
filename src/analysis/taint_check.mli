(** Report outputs (%Q) driven by untrusted inputs (%I, %M) without a bounds check. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
