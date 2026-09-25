(** Detect outputs, memory and global variables written by program instances
    that run in different tasks. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
