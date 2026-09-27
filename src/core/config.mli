(** Configuration values including platform and implementation dependent options.

    When no [iec_checker.json] file is found, {!default} is used and the
    analyzer behaves identically to a build without configuration support. *)

(** The full configuration record. *)
type t = {
  disabled_detectors : string list;
  enabled_detectors  : string list;
  mccabe_complexity  : int;
  mccabe_variant     : string;
  (** "standard" McCabe complexity, or "plcopen", weighted to reproduce the
      examples of PLCopen rule CP9 *)
  statements_count   : int;
  max_string_length  : int;
  duplicate_code_size : int;
  (** Minimum size of duplicated code to report, in syntax tree nodes *)
  output_format      : string;
  use_color          : bool;
  min_severity       : string; (** Hide warnings below: "low", "medium" or "high" *)
  min_plcopen_importance : string;
  (** If set, only report PLCopen rules of at least this importance *)
  input_format       : string;
  merge              : bool;
  exclude_paths      : string list;
  dump               : bool;
  verbose            : bool;

  (* Naming conventions; for PLCOpen-N detectors *)
  naming_type_prefixes : (string * string) list;
  (** Prefixes by type: elementary types, kinds of user-defined types
      (STRUCT, ENUM, ...), FUNCTION_BLOCK, or names of user-defined types *)
  naming_case_variable : string option;
  naming_case_constant : string option;
  naming_case_pou      : string option;
  naming_case_type     : string option;
  naming_case_member   : string option;
  (** For struct members; the variable style if not set *)
  naming_case_enum_value : string option;
  (** For enum values; the constant style if not set *)
  naming_min_length    : int;
  naming_min_length_local : int;
  (** For local variables of POUs; [naming_min_length] if 0 *)
  naming_max_length    : int;
  naming_udt_prefixes  : (string * string) list; (** Keys are among {!udt_kinds} *)
  naming_scope_prefixes : (string * string) list; (** Keys are among {!scopes} *)
  naming_allow_non_ascii : bool;
  (** Allow characters outside ASCII in names, such as a national character
      set (PLCopen N8) *)
}

val default : t

val case_styles : string list
(** Case styles of naming conventions. *)

val udt_kinds : string list
(** Keys of the prefixes of user-defined types and POUs. *)

val scopes : string list
(** Keys of the prefixes of variables by scope. *)

val canonical_type_key : string -> string
(** Upper-cased key of a prefix, with long names for types that have two:
    TOD is TIME_OF_DAY. *)
(** Default configuration — matches the original hardcoded values. *)

val set : t -> unit
(** Set the global configuration.  Must be called exactly once, before any
    analysis pass runs. *)

val get : unit -> t
(** Return the current global configuration. *)

(** {2 Backward-compatible accessors} *)

val max_string_len : unit -> int
(** Maximum size of STRING and WSTRING data types. *)

val mccabe_complexity_threshold : unit -> int
(** Threshold of McCabe complexity to generate warnings. *)

val statements_num_threshold : unit -> int
(** Threshold of maximum number of statements in POU to generate warnings. *)

val duplicate_code_size : unit -> int
(** Minimum size of duplicated code to report, in syntax tree nodes. *)

(** {2 Config file I/O} *)

val load_file : string -> (t, string) result
(** [load_file path] reads [path] as JSON and merges it onto {!default}. *)

val find_config_file : string -> string option
(** [find_config_file dir] walks from [dir] up to the filesystem root looking
    for [iec_checker.json].  Returns [Some path] or [None]. *)

val to_yojson : t -> Yojson.Safe.t
(** Serialize a configuration to JSON (for [--dump-config] / [--generate-config]). *)
