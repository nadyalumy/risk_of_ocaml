(** Implements the flow and phases of the game. *)

type player = Player.t
(** [player] is a player in the game *)

type players = player list
(** [players] is a list of players that are currently in the game *)

(** [phase] is the current phase of the game *)
type phase =
  | Deploy
  | Attack
  | Fortify

type t
(** [t] The type of the current game *)

val init : string list -> int -> t
(** [init lst n] Given a string list [lst] of player names and int [n]
    representing the number of players, initialize the game*)

val get_players : t -> player list
(** [get_players game] Given a game [game], returns the players in this game as
    a [player list]. *)

val get_current_player : t -> player
(** [get_current_player g] Given a game [g], return the current player*)

val next_player : t -> player
(** [netx_player g] Given a game [g], return the next player*)

val get_phase : t -> phase
(** [get_phase g] Given a game [g], return the current phase*)

val get_territories : t -> Territories.t array
(** [get_territories g] Given a game [g], return the [Territories.t array]*)

val get_remaining_troops : t -> int
(** [get_remaining_troops g] Given a game [g], return the remaining troops to be
    deployed. *)

val change_phase : t -> t
(** [change_phase g] Given a game [g], return a game that has moved onto the
    next phase of the game *)

val get_game_over : t -> bool
(** [get_game_over g] Given a game [g], return [true] if the game is over and
    [false] otherwise *)
