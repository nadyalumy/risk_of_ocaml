(** Implements the continents in the game map. *)

type t
(** [t] is the representation of a continent*)

(** [continents] is the type of a continent *)
type continents =
  | Europe
  | Asia
  | Africa
  | Australia
  | North_America
  | South_America

val get_name : t -> continents
(** [get_name c] Given a continent [c], return the type of the continent *)

val get_value : t -> int
(** [get_value c] Given a continent [c], return the value of bonus points a
    player can obtain from owning the continent. *)

val to_string : continents -> string
(** [to_string c] Given a continent [c], return the string of the continent *)

val get_continent_number : continents -> int
(** [to_string c] Given a continent [c], return the number of territories in the
    continent *)

val get_continent_value : continents -> int
(** [to_string c] Given a continent [c], return the int value of the continent *)

val of_string : string -> t
(** [of_string str] Given a string [str], return the associated type of the
    continent *)
