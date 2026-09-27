(** PLCOPEN-CP25: Data type conversion should be explicit. *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Warn.t list
val detector : Detector.t
