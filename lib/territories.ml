module Y = Yojson.Basic.Util
module C = Continent
module R = Raylib

(** AF: The record {name : _; mutable troops : _; continent : _; location_x : _;
  location_y : _; neighbours : _ ; mutable owner : _;} represents a game. 
  RI: Neighbours must never be empty and there should always be > 0 troops on a
  territory. *)

type owner = string

type t = {
  name : string;
  mutable troops : int;
  continent : Continent.t;
  location_x : int;
  location_y : int;
  neighbours : string list;
  mutable owner : owner;
}

(* * Function to check the representation invariant let rep_ok (t : t) : unit =
   let chk1 = t.troops > 0 in let chk2 = List.length t.neighbours <> 0 in if
   chk1 && chk2 then () else failwith "rep invariant violated" *)

let change_owner c s = c.owner <- s
let get_owner c = c.owner
let get_name c = c.name
let get_troops c = c.troops
let get_continent c = c.continent
let get_location c = (c.location_x, c.location_y)
let get_neighbours c = c.neighbours

let neighbours_to_string c =
  List.fold_left (fun acc t -> acc ^ t) "" c.neighbours

let init json : t =
  let continent_str = json |> Y.member "continent" |> Y.to_string in
  let continent =
    try Continent.of_string continent_str
    with Failure _ -> failwith "Invalid continent in JSON"
  in
  {
    name = json |> Y.member "name" |> Y.to_string;
    troops = 0;
    continent;
    location_x = json |> Y.member "location_x" |> Y.to_int;
    location_y = json |> Y.member "location_y" |> Y.to_int;
    neighbours =
      json |> Y.member "neighbors" |> Y.to_list |> List.map Y.to_string;
    owner = "";
  }

let add_value (n : int) (territory : t) : unit =
  territory.troops <- territory.troops + n

let subtract_value (n : int) (territory : t) : unit =
  territory.troops <- territory.troops - n
