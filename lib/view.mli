(** Handles input values from the GUI. *)

val value_from_gui : string option ref
(** [value_from_gui] is the value from the GUI *)

val get_value_from_gui : unit -> string option
(** [get_value_from_gui u] gets the value input from the GUI *)
