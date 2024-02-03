(** Established the textures and frames of the GUI for the active game. *)

open Raylib

type t
(** [t] represents textures of active page *)

val initialize_active : unit -> unit
(** [initialize_active u] Given unit [u], loads the textures of the active page. *)

val draw_active : Vector2.t -> unit
(** [draw_active v] Given a vector 2 [v], draws a frame of the active page. *)
