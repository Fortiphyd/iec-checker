(** Detect duplicated code, and names in copies that look like copy-paste
    errors. *)
open IECCheckerCore
module S = Syntax

val run :
  ?min_size:int ->
  duplicates:bool ->
  inconsistent:bool ->
  S.iec_library_element list ->
  Warn.t list
(** [run ~duplicates ~inconsistent elements] reports duplicated code with id
    [DuplicateCode] if [duplicates] is set, and names that break the renaming
    of a copy with id [InconsistentCopy] if [inconsistent] is set. Copies
    smaller than [min_size] syntax tree nodes are ignored; the default comes
    from the configuration. *)
