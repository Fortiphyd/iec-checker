(** Cyclomatic complexity of POUs. *)
open IECCheckerCore
module S = Syntax

val mccabe : S.iec_library_element -> int
(** [mccabe pou] McCabe cyclomatic complexity of [pou]: the number of
    decisions (IF, ELSIF, CASE selections and loops) plus one. *)
