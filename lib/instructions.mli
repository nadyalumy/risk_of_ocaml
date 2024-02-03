(** Implements the GUI instructions page at the start of the game. *)

open Raylib

type t
(** [t] represents textures of instructions page *)

val initialize_instructions : unit -> unit
(** [initialize_instructions u] Given unit [u], loads the textures of the
    instructions page. *)

val get_button_hl : unit -> Texture2D.t
(** [get_button_hl u] Given a unit [u], return the button if the instructions
    page is active. *)

val draw_instructions : Vector2.t -> unit
(** [draw_instructions v] Given a vector 2 [v], draws a frame of the
    instructions page. *)
