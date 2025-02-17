(** Most basic *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
