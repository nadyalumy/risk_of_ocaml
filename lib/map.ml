module Y = Yojson.Basic.Util
module R = Raylib

type territories = Territories.t array
type continents = Continent.t array
type t = { territories : territories (* continents : continents; *) }

(* let get_continents map = map.continents *)
let get_territories map = map.territories

let create_map path : t =
  {
    territories =
      path |> Y.member "territories" |> Y.to_list |> Array.of_list
      |> Array.map Territories.init;
    (* continents = path |> Y.member "Continent" |> Y.to_list |> Array.of_list
       |> Array.map Continent.create; *)
  }
