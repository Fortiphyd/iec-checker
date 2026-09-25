(** Output interfaces for static analysis warnings. *)
module W = Warn

type output_format =
  | Plain
  | Json
  | Sarif (** SARIF 2.1.0 *)

(** Description of a check, for output formats that list them. *)
type rule = {
  rule_id : string;
  rule_name : string;
  help_url : string;
  rule_severity : W.severity;
}

val print_report :
  ?doc_urls:(string * string) list ->
  ?use_color:bool ->
  ?rules:rule list ->
  W.t list -> output_format -> unit
(** [print_report] Print warnings in selected format to stdout. [rules] are
    only used by [Sarif], which must be printed once for all input files. *)
