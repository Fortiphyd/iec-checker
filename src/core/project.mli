(** Where the project and its documentation live. *)

val repository_url : string

val docs_url : string
(** Base URL of the documentation, in the repository's [docs] directory. *)

val check_doc_url : string -> string
(** [check_doc_url id] Documentation of the check [id] in the detectors
    reference. *)
