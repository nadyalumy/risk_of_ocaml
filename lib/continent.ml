module Y = Yojson.Basic.Util

type continents =
  | Europe
  | Asia
  | Africa
  | Australia
  | North_America
  | South_America

(** AF: The record {  name : _ ; value : _ } represents a continent. 
  RI: None *)

type t = {
  name : continents;
  value : int;
}

let get_name t = t.name
let get_value t = t.value

let to_string = function
  | Europe -> "Europe"
  | Asia -> "Asia"
  | Africa -> "Africa"
  | Australia -> "Australia"
  | North_America -> "North America"
  | South_America -> "South America"

let get_continent_number = function
  | Europe -> 7
  | Asia -> 12
  | Africa -> 6
  | Australia -> 4
  | North_America -> 9
  | South_America -> 4

let get_continent_value = function
  | Europe -> 5
  | Asia -> 7
  | Africa -> 3
  | Australia -> 2
  | North_America -> 5
  | South_America -> 2

let of_string = function
  | "Europe" -> { name = Europe; value = 5 }
  | "Asia" -> { name = Asia; value = 7 }
  | "Africa" -> { name = Africa; value = 3 }
  | "Australia" -> { name = Australia; value = 2 }
  | "North America" -> { name = North_America; value = 5 }
  | "South America" -> { name = South_America; value = 2 }
  | _ -> failwith "Invalid continent string"

(* let create json : t = let name_str = json |> Y.member "name" |> Y.to_string
   in let name = try of_string name_str with Failure _ -> failwith "Invalid\n
   continent in JSON" in { name; value = json |> Y.member "value" |> Y.to_int
   } *)
(*let ownsV2 (lst : territory list) (p : player) : int = 0 *)
