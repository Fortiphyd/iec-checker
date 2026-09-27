(** Complexity of POUs. *)
open IECCheckerCore
module S = Syntax

val mccabe : S.iec_library_element -> int
(** [mccabe pou] McCabe cyclomatic complexity of [pou]: the number of
    decisions (IF, ELSIF, CASE selections and loops) plus one. *)

val mccabe_plcopen : S.iec_library_element -> int
(** [mccabe_plcopen pou] McCabe complexity weighted to reproduce the values
    of the examples of PLCopen rule CP9: as {!mccabe}, and each AND and OR in
    a condition counts 1, a FOR loop 2 and each EXIT 1. *)

val statements : S.iec_library_element -> int
(** [statements pou] Number of statements of [pou], each counted once:
    assignments, calls, IF, each ELSIF, CASE, loops, EXIT, CONTINUE and
    RETURN. Conditions and calls in expressions aren't statements of their
    own, and neither is ELSE. *)
