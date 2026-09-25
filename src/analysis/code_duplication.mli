(** Hash statements of POUs to find duplicated code. Not wired into the driver yet. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
