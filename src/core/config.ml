(** Configuration values including platform and implementation dependent options.

    When no [iec_checker.json] file is found, {!default} is used and the
    analyzer behaves identically to a build without configuration support. *)

open Core

type t = {
  disabled_detectors : string list;
  enabled_detectors  : string list;
  mccabe_complexity  : int;
  statements_count   : int;
  max_string_length  : int;
  duplicate_code_size : int;
  output_format      : string;
  use_color          : bool;
  min_severity       : string;
  min_plcopen_importance : string;
  input_format       : string;
  merge              : bool;
  exclude_paths      : string list;
  dump               : bool;
  verbose            : bool;
  naming_type_prefixes : (string * string) list;
  naming_case_variable : string option;
  naming_case_constant : string option;
  naming_case_pou      : string option;
  naming_case_type     : string option;
  naming_case_member   : string option;
  naming_case_enum_value : string option;
  naming_min_length    : int;
  naming_min_length_local : int;
  naming_max_length    : int;
  naming_udt_prefixes  : (string * string) list;
  naming_scope_prefixes : (string * string) list;
  naming_allow_non_ascii : bool;
}

let default = {
  disabled_detectors = [];
  enabled_detectors  = [];
  mccabe_complexity  = 15;
  statements_count   = 25;
  max_string_length  = 4096;
  duplicate_code_size = 25;
  output_format      = "plain";
  use_color          = true;
  min_severity       = "low";
  min_plcopen_importance = "";
  input_format       = "st";
  merge              = false;
  exclude_paths      = [];
  dump               = false;
  verbose            = false;
  naming_type_prefixes = [];
  naming_case_variable = None;
  naming_case_constant = None;
  naming_case_pou      = None;
  naming_case_type     = None;
  naming_case_member   = None;
  naming_case_enum_value = None;
  naming_min_length    = 0;
  naming_min_length_local = 0;
  naming_max_length    = 0;
  naming_udt_prefixes  = [];
  naming_scope_prefixes = [];
  naming_allow_non_ascii = false;
}

(* ---------- Naming conventions ------------------------------------------- *)

let case_styles =
  ["UpperCamelCase"; "lowerCamelCase"; "UPPER_SNAKE_CASE"; "lower_snake_case";
   "alllowercase"; "ALLUPPERCASE"]

let udt_kinds =
  ["ENUM"; "NAMED"; "SUBRANGE"; "ARRAY"; "STRUCT"; "REF"; "ALIAS"; "UDT";
   "FUNCTION"; "FUNCTION_BLOCK"; "PROGRAM"; "CLASS"; "INTERFACE"]

let scopes = ["GLOBAL"; "LOCAL"; "TEMP"; "INPUT"; "OUTPUT"; "IN_OUT"; "PARAMETER"]

let canonical_type_key k =
  match String.uppercase k with
  | "TOD" -> "TIME_OF_DAY"
  | "LTOD" -> "LTIME_OF_DAY"
  | "DT" -> "DATE_AND_TIME"
  | "LDT" -> "LDATE_AND_TIME"
  | "REFERENCE" -> "REF"
  | "FB" -> "FUNCTION_BLOCK"
  | k -> k

(* Mutable global — set once in the binary entry point before analysis. *)
let current = ref default

let set c = current := c
let get () = !current

(* Backward-compatible accessors *)
let max_string_len () = (!current).max_string_length
let mccabe_complexity_threshold () = (!current).mccabe_complexity
let statements_num_threshold () = (!current).statements_count
let duplicate_code_size () = (!current).duplicate_code_size

(* ---------- JSON helpers ------------------------------------------------- *)

let string_list_of_yojson = function
  | `List items ->
    Ok (List.filter_map items ~f:(function `String s -> Some s | _ -> None))
  | `Null -> Ok []
  | _ -> Error "expected a JSON array of strings"

let member key json =
  match json with
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal key
  | _ -> None

let string_field json key ~default:d =
  match member key json with
  | Some (`String s) -> s
  | _ -> d

let int_field json key ~default:d =
  match member key json with
  | Some (`Int i) -> i
  | _ -> d

let bool_field json key ~default:d =
  match member key json with
  | Some (`Bool b) -> b
  | _ -> d

let string_list_field json key ~default:d =
  match member key json with
  | Some v -> (match string_list_of_yojson v with Ok l -> l | Error _ -> d)
  | None -> d

let string_map_field json key ~default:d =
  match member key json with
  | Some (`Assoc fs) ->
    List.filter_map fs ~f:(function (k, `String v) -> Some (k, v) | _ -> None)
  | _ -> d

let string_opt_field json key =
  match member key json with
  | Some (`String s) -> Some s
  | _ -> None

(* ---------- Deserialization ---------------------------------------------- *)

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

let case_style_field json key =
  Option.map (string_opt_field json key) ~f:(fun style ->
      if List.mem case_styles style ~equal:String.equal then style
      else invalid "naming_conventions.case.%s: unknown style %S; expected one of %s"
          key style (String.concat ~sep:", " case_styles))

(** A map of prefixes whose keys are canonical and, if [allowed] is given,
    among them. *)
let prefix_map_field ?allowed json key =
  string_map_field json key ~default:[]
  |> List.map ~f:(fun (k, v) ->
      let k = canonical_type_key k in
      Option.iter allowed ~f:(fun allowed ->
          if not (List.mem allowed k ~equal:String.equal) then
            invalid "naming_conventions.%s: unknown key %S; expected one of %s"
              key k (String.concat ~sep:", " allowed));
      (k, v))

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let detectors  = Option.value (member "detectors"  json) ~default:`Null in
    let thresholds = Option.value (member "thresholds" json) ~default:`Null in
    let output     = Option.value (member "output"     json) ~default:`Null in
    let input      = Option.value (member "input"      json) ~default:`Null in
    let analysis   = Option.value (member "analysis"   json) ~default:`Null in
    let naming     = Option.value (member "naming_conventions" json) ~default:`Null in
    let naming_case = Option.value (member "case" naming) ~default:`Null in
    Ok {
      disabled_detectors = string_list_field detectors "disabled"  ~default:default.disabled_detectors;
      enabled_detectors  = string_list_field detectors "enabled"   ~default:default.enabled_detectors;
      mccabe_complexity  = int_field thresholds "mccabe_complexity" ~default:default.mccabe_complexity;
      statements_count   = int_field thresholds "statements_count"  ~default:default.statements_count;
      max_string_length  = int_field thresholds "max_string_length" ~default:default.max_string_length;
      duplicate_code_size = int_field thresholds "duplicate_code_size" ~default:default.duplicate_code_size;
      output_format      = string_field output "format"    ~default:default.output_format;
      use_color          = bool_field   output "color"     ~default:default.use_color;
      min_severity       = string_field output "min_severity" ~default:default.min_severity;
      min_plcopen_importance =
        string_field output "min_plcopen_importance" ~default:default.min_plcopen_importance;
      input_format       = string_field input  "format"    ~default:default.input_format;
      merge              = bool_field   input  "merge"     ~default:default.merge;
      exclude_paths      = string_list_field input "exclude_paths" ~default:default.exclude_paths;
      dump               = bool_field analysis "dump"    ~default:default.dump;
      verbose            = bool_field analysis "verbose" ~default:default.verbose;
      naming_type_prefixes = prefix_map_field naming "type_prefixes";
      naming_case_variable = case_style_field naming_case "variable";
      naming_case_constant = case_style_field naming_case "constant";
      naming_case_pou      = case_style_field naming_case "pou";
      naming_case_type     = case_style_field naming_case "type";
      naming_case_member   = case_style_field naming_case "struct_member";
      naming_case_enum_value = case_style_field naming_case "enum_value";
      naming_min_length    = int_field naming "min_length" ~default:default.naming_min_length;
      naming_min_length_local =
        int_field naming "min_length_local" ~default:default.naming_min_length_local;
      naming_max_length    = int_field naming "max_length" ~default:default.naming_max_length;
      naming_udt_prefixes  = prefix_map_field ~allowed:udt_kinds naming "udt_prefixes";
      naming_scope_prefixes = prefix_map_field ~allowed:scopes naming "scope_prefixes";
      naming_allow_non_ascii =
        bool_field naming "allow_non_ascii" ~default:default.naming_allow_non_ascii;
    }
  with
  | Invalid msg -> Error msg
  | exn ->
    Error (Printf.sprintf "Failed to parse configuration: %s" (Exn.to_string exn))

(* ---------- Serialization ------------------------------------------------ *)

let to_yojson (c : t) : Yojson.Safe.t =
  let string_map_to_json pairs =
    `Assoc (List.map pairs ~f:(fun (k, v) -> (k, `String v)))
  in
  let opt_string o = match o with Some s -> `String s | None -> `Null in
  `Assoc [
    "detectors", `Assoc [
      "disabled", `List (List.map c.disabled_detectors ~f:(fun s -> `String s));
      "enabled",  `List (List.map c.enabled_detectors  ~f:(fun s -> `String s));
    ];
    "thresholds", `Assoc [
      "mccabe_complexity", `Int c.mccabe_complexity;
      "statements_count",  `Int c.statements_count;
      "max_string_length", `Int c.max_string_length;
      "duplicate_code_size", `Int c.duplicate_code_size;
    ];
    "output", `Assoc [
      "format", `String c.output_format;
      "color",  `Bool   c.use_color;
      "min_severity", `String c.min_severity;
      "min_plcopen_importance", `String c.min_plcopen_importance;
    ];
    "input", `Assoc [
      "format",        `String c.input_format;
      "merge",         `Bool   c.merge;
      "exclude_paths", `List (List.map c.exclude_paths ~f:(fun s -> `String s));
    ];
    "analysis", `Assoc [
      "dump",    `Bool c.dump;
      "verbose", `Bool c.verbose;
    ];
    "naming_conventions", `Assoc [
      "type_prefixes", string_map_to_json c.naming_type_prefixes;
      "case", `Assoc [
        "variable", opt_string c.naming_case_variable;
        "constant", opt_string c.naming_case_constant;
        "pou",      opt_string c.naming_case_pou;
        "type",     opt_string c.naming_case_type;
        "struct_member", opt_string c.naming_case_member;
        "enum_value", opt_string c.naming_case_enum_value;
      ];
      "min_length", `Int c.naming_min_length;
      "min_length_local", `Int c.naming_min_length_local;
      "max_length", `Int c.naming_max_length;
      "udt_prefixes", string_map_to_json c.naming_udt_prefixes;
      "scope_prefixes", string_map_to_json c.naming_scope_prefixes;
      "allow_non_ascii", `Bool c.naming_allow_non_ascii;
    ];
  ]

(* ---------- File I/O ----------------------------------------------------- *)

let load_file path =
  try
    let json = Yojson.Safe.from_file path in
    of_yojson json
  with
  | Yojson.Json_error msg ->
    Error (Printf.sprintf "Failed to parse %s: %s" path msg)
  | Sys_error msg ->
    Error (Printf.sprintf "Cannot read %s: %s" path msg)

let config_filename = "iec_checker.json"

let find_config_file start_dir =
  let rec walk dir =
    let candidate = Filename.concat dir config_filename in
    if Stdlib.Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None  (* reached root *)
      else walk parent
  in
  walk start_dir
