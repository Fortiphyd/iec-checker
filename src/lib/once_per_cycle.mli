(** Find things that should happen at most once per PLC cycle, such as
    writes to a physical output, but can happen more often on some path
    through a POU.

    Paths follow the control flow: events in exclusive IF/CASE branches are
    on different paths, events after RETURN are on no path, and a loop body
    can run several times. *)
open IECCheckerCore

type event = {
  key : string; (** Identifies what should happen once *)
  shown : string; (** Name to show in warnings *)
  ti : Tok_info.t;
}

type repeat = {
  event : event; (** The repeated event *)
  first : Tok_info.t; (** Position of the first event with the same key *)
  in_loop : bool; (** The event repeats itself in a loop *)
}

val find_repeated :
  events:(Syntax.statement -> event list) -> Syntax.statement list -> repeat list
(** [find_repeated ~events stmts] returns the events that can happen again
    on a path through [stmts] after an event with the same key, ordered by
    position. [events] is called on assignment and call statements. *)
