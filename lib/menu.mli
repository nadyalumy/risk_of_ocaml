(** Implements the GUI menu at the start of the game. *)

open Raylib

type t
(** [t] represents textures of menu page *)

val string_compare : string -> string -> bool
(** [string_compare s1 s2] String comparison for s1 and s2 because
    Stdlib.compare and String.compare did not work for Raylib text box outpus.*)

val initialize_menu : unit -> unit
(** [initialize_menu u] Given unit [u], loads the textures of the menu page. *)

val draw_menu : Vector2.t -> unit
(** [draw_menu v] Given a vector 2 [v], draws a frame of the menu page. *)
