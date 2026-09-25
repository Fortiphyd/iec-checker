(** Flag variables assigned from located (AT %...) variables that are never written in the POU. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
