(** [value_from_gui] the value from the GUI *)
let value_from_gui : string option ref = ref None

(** [get_value_from_gui ()] gets the value input from the GUI *)
let get_value_from_gui () = !value_from_gui
