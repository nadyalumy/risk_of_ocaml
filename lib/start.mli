(** Implements the GUI features for the starting pages of the game. *)

open Raylib

type t
(** [t] represents textures of start page *)

val initialize_start : unit -> unit
(** [initialize_start u] Given unit [u], loads the textures of the start page. *)

val get_button_hl : unit -> Texture2D.t
(** [get_button_hl u] Given a unit [u], return the button if the start page is
    active. *)

val draw_start : Vector2.t -> unit
(** [draw_start v] Given a vector 2 [v], draws a frame of the start page. *)
