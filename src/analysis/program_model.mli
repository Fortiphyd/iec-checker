(** What programs do across POUs: the outputs, memory and global variables
    they write and the global function block instances they call, including
    through the function blocks and functions they call, and the tasks their
    instances run in. *)
open IECCheckerCore
module S = Syntax

type target =
  | Global of string * bool
  (** A global variable or FB instance, and whether it was declared
      VAR_EXTERNAL rather than used without a declaration *)
  | Address of string (** A directly represented output or memory address *)

type access = {
  target : target;
  ti : Tok_info.t; (** Where the access is *)
  pou : string; (** POU that contains the access *)
}

type effects = {
  writes : access list;
  calls : access list; (** Calls of global function block instances *)
}

val effects : S.iec_library_element list -> string -> effects
(** [effects elements] Effects of the POU with the given name, including those
    of the POUs it calls. *)

type instance = {
  name : string;
  type_name : string option; (** Program type *)
  task : string; (** Task, qualified by the resource if it has a name *)
  resource : S.resource_decl;
}

val instances : S.configuration_decl -> instance list
(** Program instances of a configuration, in declaration order. Programs
    without a task run in the background, which counts as a task named
    "(none)". *)

type resolved = {
  key : string; (** Identifies the variable within the configuration *)
  global : string option; (** Name of the global variable *)
  address : string option; (** Address, for located variables *)
}

val resolve : S.configuration_decl -> S.resource_decl -> target -> resolved option
(** [resolve config resource target] The variable [target] refers to in a
    program of [resource], or [None] if it isn't a global or an address. *)

type conflict = {
  resolved : resolved;
  access : access;
  instance : instance;
  other : instance; (** The first instance of the task with such an access *)
}

val same_task :
  S.iec_library_element list ->
  select:(effects -> access list) ->
  keep:(resolved -> bool) ->
  conflict list
(** [same_task elements ~select ~keep] Accesses, among those [select] picks
    from the effects of each program instance, to a variable [keep] accepts
    that another instance in the same task also makes. Instances in one task
    run one after another in each cycle. Only the accesses of instances after
    the first are returned. *)
