(** Implements the territories in the map of the game. *)

type t
(** type [t] corresponds to a territory *)

val change_owner : t -> string -> unit
(** [change_owner t p] Mutates [t.owner] to equal [p].*)

val get_owner : t -> string
(** [get_owner t] Returns the owner of the territory.*)

val get_name : t -> string
(** [get_name t] Given a territory [t], return the name of the territory *)

val get_troops : t -> int
(** [get_value t] Given a territory [t], return its number of troops. *)

val get_continent : t -> Continent.t
(** [get_continent t] Given a territory [t], return the continent it is in *)

val get_location : t -> int * int
(** [get_location t] Given a territory [t], return the location of the territory
    on the screen *)

val get_neighbours : t -> string list
(** [get_neighbours t] Given a territory [t], return its neighbors *)

val neighbours_to_string : t -> string
(** [neighbours_to_string p] Given a territory [p], returns its neighboors in a
    string form *)

val init : Yojson.Basic.t -> t
(** [init n c] initializes a territory with values name = n, continent = c,
    neighbours default to empty list [], troops default to 0, location defaults
    to 0 *)

val add_value : int -> t -> unit
(** [add_value n territory] Given the number of troops to be added [n] and a
    territory [territory], increase the number of troops in [territory] by [n]. *)

val subtract_value : int -> t -> unit
(** [subtract_value n territory] Given the number of troops to be subtracted [n]
    and a territory [territory], decrease the number of troops in [territory] by
    [n]. *)
