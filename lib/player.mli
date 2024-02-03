(** Implements the players in the game. *)

type t
(** [t] Represents a player *)

val get_name : t -> string
(** [get_name p] Given a player [p] returns the name of [p]. *)

val get_color : t -> Raylib.Color.t
(** [get_color p] Given a player [p] returns the color of [p]. *)

val get_territory : t -> string -> Territories.t
(** [get_territory p s] Given a player [p] and a string [s], returns the
    territory with Territory.name = s. Raises: ["not owned"] if not found *)

val get_territories : t -> Territories.t option array
(** [get_territories p] Given a player [p] returns the Territories owned by [p]. *)

val get_territories_lst : t -> Territories.t list
(** [get_territories_lst p] Given a player [p] returns the list of Territories
    owned by [p].*)

val add_territory : t -> Territories.t -> unit
(** [add_territory p c] Given a player [p] and territory [c], add the territory
    to the player [p]. *)

val remove_territory : t -> Territories.t -> unit
(** [remove_territory p c] Given a player [p] and territory [c], remove the
    territory from the player [p]. Requires: [c] must be owned by the player*)

val num_territories : t -> int
(** [num_territories p] Given a player [p] returns the number of Territories
    owned by [p]. *)

val get_continent_bonus : t -> int
(** [get_continent_bonus p] Given a player [p], returns bonus recieved by the
    player from continents they own. Possible continents are "Asia","North
    America","Africa","South America","Europe", and "Australia"*)

val territories_to_string : t -> string
(** [territories_to_string p] Given a player [p], returns the territories owned
    by player [p] in a string form*)

val init : string -> Raylib.Color.t -> t
(** [init n c] Initializes a player given a name [string] and color
    [Raylib.Color.t].*)
