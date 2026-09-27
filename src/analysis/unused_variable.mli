(** Detect unused variables in the source code: local variables of POUs, and
    global variables of configurations (PLCopen CP24). *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
