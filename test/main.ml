(* -------------------------- Testing Approach --------------------------- *)
(* We used two approaches in testing our system. These two approaches include
   testing using OUnit tests and manual testing by playing our game step-by-step
   by running “make play” on the terminal. OUnit tests were used to test the
   following modules: Continent, Map, Player, and Territories. The remaining
   modules such as Game, Active, Constants, Start, Instructions, View and Menu
   and the game/main.ml file were tested manually through “make play”. Helper
   functions not included in the signature of these modules were tested through
   Utop.

   OUnit tests: For OUnit tests, we tested all modules that contain functions
   supporting our Game module. Our Game module is the main module that is
   responsible for running the flow and phases of our game. This Game module
   relies on the following modules: Continent, Map, Territories, and Player.
   Hence, OUnit tests were used to test these 4 modules.

   For the Continent module, we mainly used glass box testing. The Continent
   module comprises functions that create the continents in our game and extract
   information about each continent such as its name, the number of territories
   in the continent, and the bonus value given to each player for owning the
   entire continent. Hence, the implementation of almost all functions in the
   Continent module includes having multiple branches to handle each of the
   continents in our game. Thus, we used glass box testing to test the functions
   in the Continent module to ensure good coverage and that all functions work
   on each continent that exists in our game.

   To test this module, we used the Continent.of_string function to create an
   instance of each continent in the game. Then, we tested the remaining
   functions in the module on each of these instantiated continents through
   glass box testing. Hence, we believe that we have achieved good coverage of
   the functions on the Continent module. We ensured that each function produces
   the expected outputs on each input continent. Thus, we believe that our
   Continent module has been implemented correctly and successfully.

   For the Map, Territories, and Player modules, we mainly used black box
   testing. With these modules, most functions are not implemented to go through
   multiple branches based on differing inputs. Hence, most functions in these
   modules are designed to take similar paths and behaviors on all inputs. Thus,
   we used black box testing for the functions in these modules as we were not
   too concerned regarding testing multiple branches. We simply wanted to make
   sure that all of these functions produce the correct output and behavior on
   each chosen input.

   We tested the Map module by using the functions in it to create an instance
   of each territory in the game. Then, we tested the functions in the
   Territories module on these created territories, making sure that each
   function produced the expected output and behavior on each input territory.
   Additionally, we tested the Player module by using the Player.init function
   to create instances of multiple players in the game. We then also tested the
   remaining functions in the Player module on these created players and
   previously created territories, again ensuring that every function results in
   the correct output and behavior based on our chosen inputs. Therefore, we
   believe that our Map, Territories, and Player modules have been implemented
   correctly according to our specifications and expectations.

   Manual testing using the terminal through “make play”: Manual testing was
   used to test the game/main.ml file and Game module, which are responsible for
   running the entire flow and different phases of the game. Thus, these systems
   contain many functions that execute the game and print various information
   such as which player’s turn it is to play or how many troops are currently in
   each territory to the terminal for players to see. The Game module also
   contains many functions that use readline to ask for inputs from the players
   such as player names and how many troops a player would like to deploy to a
   certain country. As a result, these systems cannot be tested normally through
   OUnit tests. Hence, we tested the functions in these systems by running “make
   play” on the terminal and playing the game step-by-step.

   To ensure that our functions in game/main.ml and Game were implemented
   correctly, we ran “make play” many times and tried out all the possibilities
   we could think of over many games. For instance, we tried playing the game
   with different numbers of players, ranging from 2 to 6. We then made sure
   that our game loop flowed properly. For each player’s turn, the game
   progressed from the Deploy state to the Attack state to the Fortify state
   before switching players.

   At each phase during each player’s turn, we would try out different
   possibilities each time to get good coverage. For instance, we would deploy,
   attack, and fortify various countries, trying out both valid territories
   (players are allowed to deploy, attack, and fortify these territories) and
   invalid territories. We would also try deploying, attacking, and fortifying
   with various numbers of troops, ranging from the minimum and maximum number
   of troops allowed and both valid and invalid numbers of troops.

   Each time, we would check whether we see the expected results and behavior or
   not. For example, if we chose to attack Western Australia, then we would
   check that Western Australia indeed is the country that gets attacked. If we
   chose to deploy 3 troops to Indonesia, then we would check that Indonesia’s
   number of troops increased by 3. If we tried invalid inputs such as attacking
   a country that the attacking player owned, then we would check that the game
   would give us an “invalid input” warning. If we found incorrect behavior or
   results, we would then debug and fix the implementation of our functions
   accordingly.

   We repeated the above steps each time we implemented a new function. We have
   also tried playing the game until it “finishes” and one single player has
   conquered the entire map, thus winning the game. This shows that our game
   does progress and terminates correctly. Hence, we are confident that our game
   system has been implemented correctly and successfully.

   As for the Active, Constants, Start, Instructions, View and Menu modules,
   these modules comprise the functions that create and manipulate the GUI
   display of our game. Thus, these modules also cannot be tested through OUnit
   test cases. Instead, we also tested them manually by running “make play”. As
   we play the game, we would check whether all of the GUI features of the game
   are displayed properly and that they function as expected. For example, we
   would check that all territories are positioned correctly and neatly on the
   map. We would also check that all buttons function correctly and result in
   the expected behavior. For instance, if we press the button for 4 players at
   the beginning of the game, then we expect the game to solicit players to
   input 4 names for the 4 players.

   Utop Testing: Since we could not access helper functions that were not
   included in our module signatures, we could not test them normally through
   OUnit tests. Hence, we would usually copy and paste some of our code related
   to the different types and helper functions we created into Utop. We would
   then test our helper functions on Utop to make sure that they have been
   implemented correctly. Helper functions that were tested this way were
   functions that were used to query data structures such as a function that
   returns the first index that contains None in an option array.

   Overall, through the testing strategies detailed above, we believe that we
   have attained good testing coverage with all of our modules and functions,
   including those that handle the front-end GUI and back-end features of our
   game. Hence, we believe that our entire game system has been implemented
   correctly according to our expectations and requirements. *)

open OUnit2
open Lib

(***************************** Pretty Printers *******************************)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_list pp_elt arr] pretty-prints array [arr], using [pp_elt] to
    pretty-print each element of [arr]. *)
let pp_array pp_elt arr =
  let loop arr =
    let acc = ref "" in
    for i = 0 to Array.length arr - 1 do
      let str = pp_elt arr.(i) in
      if str = "" then acc := !acc else acc := !acc ^ str ^ "; "
    done;
    !acc
  in
  "{" ^ loop arr ^ "}"

let pp_country_list = pp_list Territories.get_name

let pp_country_option_array =
  pp_array (fun x ->
      match x with
      | Some y -> Territories.get_name y
      | None -> "")

(******************************** EXAMPLES ************************************)
(*--------------------------- Continent Examples -----------------------------*)

(* json file of the classic map *)
let path = "data/countries.json"
let json = Yojson.Basic.from_file path
let map = Map.create_map json

(* all territories *)
let territories = Map.get_territories map

let get_territory_from_array name =
  match Array.find_opt (fun x -> Territories.get_name x = name) territories with
  | None -> failwith "bruh"
  | Some v -> v

(* Create continents using Continent.of_string *)
let north_america = Continent.of_string "North America"
let south_america = Continent.of_string "South America"
let africa = Continent.of_string "Africa"
let europe = Continent.of_string "Europe"
let asia = Continent.of_string "Asia"
let australia = Continent.of_string "Australia"

(***************************** Continent tests ********************************)

(*----------------------------Continent.get_name -----------------------------*)
let continent_get_name_test out in1 _ =
  assert_equal out (Continent.get_name in1)

let continent_get_name_tests =
  [
    "get name of North America"
    >:: continent_get_name_test North_America north_america;
    "get name of South America"
    >:: continent_get_name_test South_America south_america;
    "get name of Africa" >:: continent_get_name_test Africa africa;
    "get name of Europe" >:: continent_get_name_test Europe europe;
    "get name of Asia" >:: continent_get_name_test Asia asia;
    "get name of Australia" >:: continent_get_name_test Australia australia;
  ]

(*----------------------------Continent.get_value ----------------------------*)
let continent_get_value_test out in1 _ =
  assert_equal out (Continent.get_value in1)

let continent_get_value_tests =
  [
    "get value of North America" >:: continent_get_value_test 5 north_america;
    "get value of South America" >:: continent_get_value_test 2 south_america;
    "get value of Africa" >:: continent_get_value_test 3 africa;
    "get value of Europe" >:: continent_get_value_test 5 europe;
    "get value of Asia" >:: continent_get_value_test 7 asia;
    "get value of Australia" >:: continent_get_value_test 2 australia;
  ]

(*----------------------------Continent.to_string ----------------------------*)
let continent_to_string_test out in1 _ =
  assert_equal out (Continent.to_string (Continent.get_name in1))

let continent_to_string_tests =
  [
    "to_string North America"
    >:: continent_to_string_test "North America" north_america;
    "to_string South America"
    >:: continent_to_string_test "South America" south_america;
    "to_string Africa" >:: continent_to_string_test "Africa" africa;
    "to_string Europe" >:: continent_to_string_test "Europe" europe;
    "to_string Asia" >:: continent_to_string_test "Asia" asia;
    "to_string Australia" >:: continent_to_string_test "Australia" australia;
  ]

(*---------------------Continent.get_continent_number -----------------------*)
let get_continent_number_test out in1 _ =
  assert_equal out (Continent.get_continent_number in1)

let get_continent_number_tests =
  [
    "get number of North America" >:: get_continent_number_test 9 North_America;
    "get number of South America" >:: get_continent_number_test 4 South_America;
    "get number of Africa" >:: get_continent_number_test 6 Africa;
    "get number of Europe" >:: get_continent_number_test 7 Europe;
    "get number of Asia" >:: get_continent_number_test 12 Asia;
    "get number of Australia" >:: get_continent_number_test 4 Australia;
  ]

(*---------------------Continent.get_continent_value -----------------------*)
let get_continent_value_test out in1 _ =
  assert_equal out (Continent.get_continent_value in1)

let get_continent_value_tests =
  [
    "get continent value of North America"
    >:: get_continent_value_test 5 North_America;
    "get continent value of South America"
    >:: get_continent_value_test 2 South_America;
    "get continent value of Africa" >:: get_continent_value_test 3 Africa;
    "get continent value of Europe" >:: get_continent_value_test 5 Europe;
    "get continent value of Asia" >:: get_continent_value_test 7 Asia;
    "get continent value of Australia" >:: get_continent_value_test 2 Australia;
  ]

(*--------------------------- Territory Examples -----------------------------*)

(* North American territories*)
let alaska = get_territory_from_array "Alaska"
let alberta = get_territory_from_array "Alberta"
let central_america = get_territory_from_array "Central America"
let eastern_us = get_territory_from_array "Eastern US"
let greenland = get_territory_from_array "Greenland"
let northwest_territory = get_territory_from_array "Northwest Territory"
let ontario = get_territory_from_array "Ontario"
let quebec = get_territory_from_array "Quebec"
let western_us = get_territory_from_array "Western US"

(* South American territories *)
let argentina = get_territory_from_array "Argentina"
let brazil = get_territory_from_array "Brazil"
let venezuela = get_territory_from_array "Venezuela"
let peru = get_territory_from_array "Peru"

(* Australian territories *)
let eastern_australia = get_territory_from_array "Eastern Australia"
let indonesia = get_territory_from_array "Indonesia"
let new_guinea = get_territory_from_array "New Guinea"
let western_australia = get_territory_from_array "Western Australia"

(* European territories *)
let great_britain = get_territory_from_array "Great Britain"
let iceland = get_territory_from_array "Iceland"
let northern_europe = get_territory_from_array "Northern Europe"
let scandinavia = get_territory_from_array "Scandinavia"
let southern_europe = get_territory_from_array "Southern Europe"
let ukraine = get_territory_from_array "Ukraine"
let western_europe = get_territory_from_array "Western Europe"

(* African territories *)
let congo = get_territory_from_array "Congo"
let east_africa = get_territory_from_array "East Africa"
let egypt = get_territory_from_array "Egypt"
let madagascar = get_territory_from_array "Madagascar"
let north_africa = get_territory_from_array "North Africa"
let south_africa = get_territory_from_array "South Africa"

(* Asian territories *)
let afghanistan = get_territory_from_array "Afghanistan"
let china = get_territory_from_array "China"
let india = get_territory_from_array "India"
let irkutsk = get_territory_from_array "Irkutsk"
let japan = get_territory_from_array "Japan"
let kamchatka = get_territory_from_array "Kamchatka"
let middle_east = get_territory_from_array "Middle East"
let mongolia = get_territory_from_array "Mongolia"
let siam = get_territory_from_array "Siam"
let siberia = get_territory_from_array "Siberia"
let ural = get_territory_from_array "Ural"
let yakutsk = get_territory_from_array "Yakutsk"

(************************* Territories & Player tests *************************)

(*----------------------------Territories.get_name -------------------------*)
let territories_get_name_test out in1 _ =
  assert_equal out (Territories.get_name in1)

let territories_get_name_tests =
  [
    "get name of Alaska" >:: territories_get_name_test "Alaska" alaska;
    "get name of Alberta" >:: territories_get_name_test "Alberta" alberta;
    "get name of Central America"
    >:: territories_get_name_test "Central America" central_america;
    "get name of Argentina" >:: territories_get_name_test "Argentina" argentina;
    "get name of Brazil" >:: territories_get_name_test "Brazil" brazil;
    "get name of Eastern Australia"
    >:: territories_get_name_test "Eastern Australia" eastern_australia;
    "get name of Indonesia" >:: territories_get_name_test "Indonesia" indonesia;
    "get name of New Guinea"
    >:: territories_get_name_test "New Guinea" new_guinea;
    "get name of Western Australia"
    >:: territories_get_name_test "Western Australia" western_australia;
    "get name of Great Britain"
    >:: territories_get_name_test "Great Britain" great_britain;
    "get name of Iceland" >:: territories_get_name_test "Iceland" iceland;
    "get name of Northern Europe"
    >:: territories_get_name_test "Northern Europe" northern_europe;
    "get name of Congo" >:: territories_get_name_test "Congo" congo;
    "get name of East Africa"
    >:: territories_get_name_test "East Africa" east_africa;
    "get name of Egypt" >:: territories_get_name_test "Egypt" egypt;
    "get name of Afghanistan"
    >:: territories_get_name_test "Afghanistan" afghanistan;
    "get name of China" >:: territories_get_name_test "China" china;
    "get name of India" >:: territories_get_name_test "India" india;
  ]

(*------------Territories.get_troops, add_value & subtract_value ----------*)
let _ = Territories.add_value 5 alaska
let _ = Territories.add_value 5 alberta
let _ = Territories.add_value 5 central_america
let _ = Territories.add_value 5 eastern_us
let _ = Territories.add_value 5 greenland
let _ = Territories.add_value 5 northwest_territory
let _ = Territories.add_value 5 ontario
let _ = Territories.add_value 5 quebec
let _ = Territories.add_value 5 western_us

let _ =
  Territories.add_value 5 argentina;
  Territories.subtract_value 3 argentina

let _ =
  Territories.add_value 5 brazil;
  Territories.subtract_value 3 brazil

let _ =
  Territories.add_value 5 venezuela;
  Territories.subtract_value 3 venezuela

let _ =
  Territories.add_value 5 peru;
  Territories.subtract_value 3 peru

let _ =
  Territories.add_value 7 eastern_australia;
  Territories.subtract_value 5 eastern_australia

let _ =
  Territories.add_value 7 indonesia;
  Territories.subtract_value 5 indonesia

let _ =
  Territories.add_value 7 new_guinea;
  Territories.subtract_value 5 new_guinea

let _ =
  Territories.add_value 7 western_australia;
  Territories.subtract_value 5 western_australia

let _ =
  Territories.add_value 10 great_britain;
  Territories.subtract_value 5 great_britain

let _ =
  Territories.add_value 10 iceland;
  Territories.subtract_value 5 iceland

let _ =
  Territories.add_value 10 northern_europe;
  Territories.subtract_value 5 northern_europe

let _ =
  Territories.add_value 10 scandinavia;
  Territories.subtract_value 5 scandinavia

let _ =
  Territories.add_value 10 southern_europe;
  Territories.subtract_value 5 southern_europe

let _ =
  Territories.add_value 10 ukraine;
  Territories.subtract_value 5 ukraine

let _ =
  Territories.add_value 10 western_europe;
  Territories.subtract_value 5 western_europe

let _ =
  Territories.add_value 1 congo;
  Territories.add_value 2 congo

let _ =
  Territories.add_value 1 east_africa;
  Territories.add_value 2 east_africa

let _ =
  Territories.add_value 1 egypt;
  Territories.add_value 2 egypt

let _ =
  Territories.add_value 1 madagascar;
  Territories.add_value 2 madagascar

let _ =
  Territories.add_value 1 north_africa;
  Territories.add_value 2 north_africa

let _ =
  Territories.add_value 1 south_africa;
  Territories.add_value 2 south_africa

let _ =
  Territories.add_value 4 afghanistan;
  Territories.add_value 3 afghanistan

let _ =
  Territories.add_value 4 china;
  Territories.add_value 3 china

let _ =
  Territories.add_value 4 india;
  Territories.add_value 3 india

let _ =
  Territories.add_value 4 irkutsk;
  Territories.add_value 3 irkutsk

let _ =
  Territories.add_value 4 japan;
  Territories.add_value 3 japan

let _ =
  Territories.add_value 4 kamchatka;
  Territories.add_value 3 kamchatka

let _ =
  Territories.add_value 4 middle_east;
  Territories.add_value 3 middle_east

let _ =
  Territories.add_value 4 mongolia;
  Territories.add_value 3 mongolia

let _ =
  Territories.add_value 4 siam;
  Territories.add_value 3 siam

let _ =
  Territories.add_value 4 siberia;
  Territories.add_value 3 siberia

let _ =
  Territories.add_value 4 ural;
  Territories.add_value 3 ural

let _ =
  Territories.add_value 4 yakutsk;
  Territories.add_value 3 yakutsk

let territories_get_troops_test out in1 _ =
  assert_equal out (Territories.get_troops in1)

let territories_troops_value_tests =
  [
    "get troops of Northwest Territory"
    >:: territories_get_troops_test 5 northwest_territory;
    "get troops of Alaska" >:: territories_get_troops_test 5 alaska;
    "get troops of Ontario" >:: territories_get_troops_test 5 ontario;
    "get troops of Quebec" >:: territories_get_troops_test 5 quebec;
    "get troops of Southern Europe"
    >:: territories_get_troops_test 5 southern_europe;
    "get troops of Ukraine" >:: territories_get_troops_test 5 ukraine;
    "get troops of Western Europe"
    >:: territories_get_troops_test 5 western_europe;
    "get troops of Madagascar" >:: territories_get_troops_test 3 madagascar;
    "get troops of North Africa" >:: territories_get_troops_test 3 north_africa;
    "get troops of South Africa" >:: territories_get_troops_test 3 south_africa;
    "get troops of Middle East" >:: territories_get_troops_test 7 middle_east;
    "get troops of Mongolia" >:: territories_get_troops_test 7 mongolia;
    "get troops of Siam" >:: territories_get_troops_test 7 siam;
    "get troops of Siberia" >:: territories_get_troops_test 7 siberia;
    "get troops of Indonesia" >:: territories_get_troops_test 2 indonesia;
    "get troops of New Guinea" >:: territories_get_troops_test 2 new_guinea;
    "get troops of Western Australia"
    >:: territories_get_troops_test 2 western_australia;
    "get troops of Brazil" >:: territories_get_troops_test 2 brazil;
    "get troops of Argentina" >:: territories_get_troops_test 2 argentina;
  ]

(*------------------------Territories.get_continent -------------------------*)
let territories_get_continent_test out in1 _ =
  assert_equal out (Territories.get_continent in1)

let territories_get_continent_tests =
  [
    "get continent of Alberta"
    >:: territories_get_continent_test north_america alberta;
    "get continent of Greenland"
    >:: territories_get_continent_test north_america greenland;
    "get continent of Ontario"
    >:: territories_get_continent_test north_america ontario;
    "get continent of Quebec"
    >:: territories_get_continent_test north_america quebec;
    "get continent of Argentina"
    >:: territories_get_continent_test south_america argentina;
    "get continent of Venezuela"
    >:: territories_get_continent_test south_america venezuela;
    "get continent of Eastern Australia"
    >:: territories_get_continent_test australia eastern_australia;
    "get continent of Western Australia"
    >:: territories_get_continent_test australia western_australia;
    "get continent of Iceland" >:: territories_get_continent_test europe iceland;
    "get continent of Scandinavia"
    >:: territories_get_continent_test europe scandinavia;
    "get continent of Western Europe"
    >:: territories_get_continent_test europe western_europe;
    "get continent of Egypt" >:: territories_get_continent_test africa egypt;
    "get continent of Madagascar"
    >:: territories_get_continent_test africa madagascar;
    "get continent of Congo" >:: territories_get_continent_test africa congo;
    "get continent of Japan" >:: territories_get_continent_test asia japan;
    "get continent of Siam" >:: territories_get_continent_test asia siam;
    "get continent of Ural" >:: territories_get_continent_test asia ural;
    "get continent of Middle East"
    >:: territories_get_continent_test asia middle_east;
  ]

(*-------------------------Territories.get_location -------------------------*)
let territories_get_location_test out in1 _ =
  assert_equal out (Territories.get_location in1)

let territories_get_location_tests =
  [
    "get location of Alaska" >:: territories_get_location_test (63, 125) alaska;
    "get location of Central America"
    >:: territories_get_location_test (207, 388) central_america;
    "get location of Ontario"
    >:: territories_get_location_test (242, 206) ontario;
    "get location of Quebec" >:: territories_get_location_test (325, 211) quebec;
    "get location of Argentina"
    >:: territories_get_location_test (295, 662) argentina;
    "get location of Venezuela"
    >:: territories_get_location_test (248, 462) venezuela;
    "get location of Peru" >:: territories_get_location_test (255, 544) peru;
    "get location of Indonesia"
    >:: territories_get_location_test (967, 604) indonesia;
    "get location of New Guinea"
    >:: territories_get_location_test (1120, 575) new_guinea;
    "get location of Eastern Australia"
    >:: territories_get_location_test (1139, 706) eastern_australia;
    "get location of Great Britain"
    >:: territories_get_location_test (498, 260) great_britain;
    "get location of Northern Europe"
    >:: territories_get_location_test (566, 284) northern_europe;
    "get location of Ukraine"
    >:: territories_get_location_test (701, 218) ukraine;
    "get location of North Africa"
    >:: territories_get_location_test (547, 525) north_africa;
    "get location of South Africa"
    >:: territories_get_location_test (639, 727) south_africa;
    "get location of Egypt" >:: territories_get_location_test (636, 474) egypt;
    "get location of Irkutsk"
    >:: territories_get_location_test (967, 199) irkutsk;
    "get location of Yakutsk"
    >:: territories_get_location_test (981, 96) yakutsk;
    "get location of Afghanistan"
    >:: territories_get_location_test (804, 298) afghanistan;
  ]

(*----------------------Territories.get_neighbours ------------------------*)
let territories_get_neighbours_test out in1 _ =
  assert_equal out (Territories.get_neighbours in1)

let territories_get_neighbours_tests =
  [
    "get neighbors of Alaska"
    >:: territories_get_neighbours_test
          [ "Northwest Territory"; "Alberta"; "Kamchatka" ]
          alaska;
    "get neighbors of Western US"
    >:: territories_get_neighbours_test
          [ "Alberta"; "Ontario"; "Eastern US"; "Central America" ]
          western_us;
    "get neighbors of Ontario"
    >:: territories_get_neighbours_test
          [
            "Northwest Territory";
            "Alberta";
            "Western US";
            "Eastern US";
            "Quebec";
            "Greenland";
          ]
          ontario;
    "get neighbors of Brazil"
    >:: territories_get_neighbours_test
          [ "Peru"; "Argentina"; "Venezuela"; "North Africa" ]
          brazil;
    "get neighbors of Venezuela"
    >:: territories_get_neighbours_test
          [ "Peru"; "Brazil"; "Central America" ]
          venezuela;
    "get neighbors of Indonesia"
    >:: territories_get_neighbours_test
          [ "Siam"; "New Guinea"; "Western Australia" ]
          indonesia;
    "get neighbors of Western Australia"
    >:: territories_get_neighbours_test
          [ "Indonesia"; "Eastern Australia"; "New Guinea" ]
          western_australia;
    "get neighbors of Northern Europe"
    >:: territories_get_neighbours_test
          [
            "Great Britain";
            "Scandinavia";
            "Western Europe";
            "Southern Europe";
            "Ukraine";
          ]
          northern_europe;
    "get neighbors of Southern Europe"
    >:: territories_get_neighbours_test
          [
            "Great Britain";
            "Western Europe";
            "Northern Europe";
            "Ukraine";
            "Middle East";
            "Egypt";
            "North Africa";
          ]
          southern_europe;
    "get neighbors of East Africa"
    >:: territories_get_neighbours_test
          [
            "Egypt";
            "South Africa";
            "Congo";
            "North Africa";
            "Madagascar";
            "Middle East";
          ]
          east_africa;
    "get neighbors of Madagascar"
    >:: territories_get_neighbours_test
          [ "East Africa"; "South Africa" ]
          madagascar;
    "get neighbors of China"
    >:: territories_get_neighbours_test
          [ "Afghanistan"; "Siam"; "India"; "Mongolia"; "Ural"; "Siberia" ]
          china;
    "get neighbors of Yakutsk"
    >:: territories_get_neighbours_test
          [ "Kamchatka"; "Irkutsk"; "Siberia" ]
          yakutsk;
  ]

(*---------------------Territories.neighbors_to_string ----------------------*)
let neighbours_to_string_test out in1 _ =
  assert_equal ~printer:(fun x -> x) out (Territories.neighbours_to_string in1)

let territories_string_neighbours_tests =
  [
    "string neighbours of Greenland"
    >:: neighbours_to_string_test "Northwest TerritoryIcelandOntarioQuebec"
          greenland;
    "string neighbours of Iceland"
    >:: neighbours_to_string_test "Great BritainScandinaviaGreenland" iceland;
    "string neighbours of Congo"
    >:: neighbours_to_string_test "East AfricaSouth AfricaNorth Africa" congo;
  ]

(*--------------------------- Player Examples --------------------------------*)
let p1 = Player.init "1" Raylib.Color.gray
let p2 = Player.init "2" Raylib.Color.green
let p3 = Player.init "3" Raylib.Color.yellow
let p4 = Player.init "4" Raylib.Color.red
let p5 = Player.init "5" Raylib.Color.blue
let p6 = Player.init "6" Raylib.Color.purple

(*----------------------------Player.get_name -------------------------*)
let player_get_name_test out in1 _ = assert_equal out (Player.get_name in1)

let player_get_name_tests =
  [
    "get name of player 1" >:: player_get_name_test "1" p1;
    "get name of player 2" >:: player_get_name_test "2" p2;
    "get name of player 3" >:: player_get_name_test "3" p3;
    "get name of player 4" >:: player_get_name_test "4" p4;
    "get name of player 5" >:: player_get_name_test "5" p5;
    "get name of player 6" >:: player_get_name_test "6" p6;
  ]

(*----------------------------Player.get_color -------------------------*)
let player_get_color_test out in1 _ = assert_equal out (Player.get_color in1)

let player_get_color_tests =
  [
    "get color of player 1" >:: player_get_color_test Raylib.Color.gray p1;
    "get color of player 2" >:: player_get_color_test Raylib.Color.green p2;
    "get color of player 3" >:: player_get_color_test Raylib.Color.yellow p3;
    "get color of player 4" >:: player_get_color_test Raylib.Color.red p4;
    "get color of player 5" >:: player_get_color_test Raylib.Color.blue p5;
    "get color of player 6" >:: player_get_color_test Raylib.Color.purple p6;
  ]

(* -----------Initialize player's territories using Player.add_territory &
   remove_territory & Territories.change_owner--------------------------------*)

(* Let player 1 own: Alaska, Quebec, Scandinavia, Japan, China, Congo *)
let _ = Player.add_territory p1 alaska
let _ = Player.add_territory p1 kamchatka
let _ = Player.add_territory p1 quebec
let _ = Player.add_territory p1 afghanistan
let _ = Player.remove_territory p1 kamchatka
let _ = Player.add_territory p1 scandinavia
let _ = Player.add_territory p1 japan
let _ = Player.add_territory p1 china
let _ = Player.add_territory p1 indonesia
let _ = Player.remove_territory p1 afghanistan
let _ = Player.add_territory p1 congo
let _ = Player.remove_territory p1 indonesia
let p1_arr = Array.make 42 None
let _ = p1_arr.(0) <- Some alaska
let _ = p1_arr.(1) <- Some congo
let _ = p1_arr.(2) <- Some quebec
let _ = p1_arr.(3) <- Some scandinavia
let _ = p1_arr.(4) <- Some japan
let _ = p1_arr.(5) <- Some china

(* Let player 2 own: Ontario, Northwest Territory, Great Britain, Kamchatka,
   Ural, South Africa, Irkutsk *)
let _ = Player.add_territory p2 ontario
let _ = Player.add_territory p2 mongolia
let _ = Player.add_territory p2 northwest_territory
let _ = Player.add_territory p2 great_britain
let _ = Player.add_territory p2 kamchatka
let _ = Player.add_territory p2 middle_east
let _ = Player.remove_territory p2 mongolia
let _ = Player.add_territory p2 ural
let _ = Player.add_territory p2 south_africa
let _ = Player.remove_territory p2 middle_east
let _ = Player.add_territory p2 indonesia
let _ = Player.add_territory p2 irkutsk
let _ = Player.remove_territory p2 indonesia
let p2_arr = Array.make 42 None
let _ = p2_arr.(0) <- Some ontario
let _ = p2_arr.(1) <- Some south_africa
let _ = p2_arr.(2) <- Some northwest_territory
let _ = p2_arr.(3) <- Some great_britain
let _ = p2_arr.(4) <- Some kamchatka
let _ = p2_arr.(5) <- Some ural
let _ = p2_arr.(6) <- Some irkutsk

(* Let player 3 own: Egypt, Madagascar, Iceland, India *)
let _ = Player.add_territory p3 greenland
let _ = Player.add_territory p3 eastern_australia
let _ = Player.add_territory p3 egypt
let _ = Player.remove_territory p3 greenland
let _ = Player.add_territory p3 madagascar
let _ = Player.add_territory p3 iceland
let _ = Player.add_territory p3 india
let _ = Player.remove_territory p3 eastern_australia
let p3_arr = Array.make 42 None
let _ = p3_arr.(0) <- Some egypt
let _ = p3_arr.(1) <- Some india
let _ = p3_arr.(2) <- Some madagascar
let _ = p3_arr.(3) <- Some iceland

(* Let player 4 own all countries in Australia: Indonesia, New Guinea, Western
   Australia, Eastern Australia *)
let _ = Player.add_territory p4 indonesia
let _ = Player.add_territory p4 new_guinea
let _ = Player.add_territory p4 western_australia
let _ = Player.add_territory p4 eastern_australia
let p4_arr = Array.make 42 None
let _ = p4_arr.(0) <- Some indonesia
let _ = p4_arr.(1) <- Some new_guinea
let _ = p4_arr.(2) <- Some western_australia
let _ = p4_arr.(3) <- Some eastern_australia

(* Let player 5 own all countries in South America: Venezuela, Brazil, Peru,
   Argentina *)
let _ = Player.add_territory p5 venezuela
let _ = Player.add_territory p5 brazil
let _ = Player.add_territory p5 peru
let _ = Player.add_territory p5 argentina
let p5_arr = Array.make 42 None
let _ = p5_arr.(0) <- Some venezuela
let _ = p5_arr.(1) <- Some brazil
let _ = p5_arr.(2) <- Some peru
let _ = p5_arr.(3) <- Some argentina

(* Let player 6 own no territories *)
let p6_arr = Array.make 42 None

(*--------------------------Player.get_territories --------------------------*)
let player_get_territories_test out in1 _ =
  assert_equal out (Player.get_territories in1)

let player_get_territories_tests =
  [
    "get territories of player 1" >:: player_get_territories_test p1_arr p1;
    "get territories of player 2" >:: player_get_territories_test p2_arr p2;
    "get territories of player 3" >:: player_get_territories_test p3_arr p3;
    "get territories of player 4" >:: player_get_territories_test p4_arr p4;
    "get territories of player 5" >:: player_get_territories_test p5_arr p5;
    "get territories of player 6" >:: player_get_territories_test p6_arr p6;
  ]

(*--------------------------Player.get_continent_bonus -----------------------*)
let player_get_continent_bonus_test out in1 _ =
  assert_equal out (Player.get_continent_bonus in1)

let player_get_continent_bonus_tests =
  [
    "get continent bonus of player 1" >:: player_get_continent_bonus_test 0 p1;
    "get continent bonus of player 2" >:: player_get_continent_bonus_test 0 p2;
    "get continent bonus of player 3" >:: player_get_continent_bonus_test 0 p3;
    "get continent bonus of player 4" >:: player_get_continent_bonus_test 2 p4;
    "get continent bonus of player 5" >:: player_get_continent_bonus_test 2 p5;
    "get continent bonus of player 6" >:: player_get_continent_bonus_test 0 p6;
  ]
(*------------------------Player.territories_to_string -----------------------*)

let player_territories_to_string_test out in1 _ =
  assert_equal ~printer:(fun x -> x) out (Player.territories_to_string in1)

let player_territories_to_string_tests =
  [
    "get string territories of player 1"
    >:: player_territories_to_string_test
          "Alaska: 5\nCongo: 3\nQuebec: 5\nScandinavia: 5\nJapan: 7\nChina: 7\n"
          p1;
    "get string territories of player 2"
    >:: player_territories_to_string_test
          "Ontario: 5\n\
           South Africa: 3\n\
           Northwest Territory: 5\n\
           Great Britain: 5\n\
           Kamchatka: 7\n\
           Ural: 7\n\
           Irkutsk: 7\n"
          p2;
    "get string territories of player 3"
    >:: player_territories_to_string_test
          "Egypt: 3\nIndia: 7\nMadagascar: 3\nIceland: 5\n" p3;
    "get string territories of player 4"
    >:: player_territories_to_string_test
          "Indonesia: 2\n\
           New Guinea: 2\n\
           Western Australia: 2\n\
           Eastern Australia: 2\n"
          p4;
    "get string territories of player 5"
    >:: player_territories_to_string_test
          "Venezuela: 2\nBrazil: 2\nPeru: 2\nArgentina: 2\n" p5;
    "get string territories of player 6"
    >:: player_territories_to_string_test "" p6;
  ]

(*----------------------------Player.get_territory -------------------------*)

let player_get_territory_test out in1 in2 _ =
  assert_equal out (Player.get_territory in1 in2)

let player_get_territory_tests =
  [
    "get japan from player 1" >:: player_get_territory_test japan p1 "jApAn";
    "get quebec from player 1" >:: player_get_territory_test quebec p1 "QUEBEC";
    "get kamchatka from player 2"
    >:: player_get_territory_test kamchatka p2 "kamchatka";
    "get south africa from player 2"
    >:: player_get_territory_test south_africa p2 "south AFRICA";
    "get egypt from player 3" >:: player_get_territory_test egypt p3 "eGyPt";
    "get iceland from player 3"
    >:: player_get_territory_test iceland p3 "ICEland";
    "get western australia from player 4"
    >:: player_get_territory_test western_australia p4 "WESTERN australia";
    "get eastern australia from player 4"
    >:: player_get_territory_test eastern_australia p4 "eastern auStrAliA";
    "get brazil from player 5" >:: player_get_territory_test brazil p5 "brAZIL";
    "get peru from player 5" >:: player_get_territory_test peru p5 "peru";
  ]

(*----------------------------Player.num_territories -------------------------*)

let player_num_territories_test out in1 _ =
  assert_equal out (Player.num_territories in1)

let player_num_territories_tests =
  [
    "num territories of player 1" >:: player_num_territories_test 6 p1;
    "num territories of player 2" >:: player_num_territories_test 7 p2;
    "num territories of player 3" >:: player_num_territories_test 4 p3;
    "num territories of player 4" >:: player_num_territories_test 4 p4;
    "num territories of player 5" >:: player_num_territories_test 4 p5;
    "num territories of player 6" >:: player_num_territories_test 0 p6;
  ]

(*--------------------------Territories.get_owner -------------------------*)

let territories_owner_test out in1 _ =
  assert_equal out (Territories.get_owner in1)

let territories_owner_tests =
  [
    "get owner of ontario" >:: territories_owner_test "2" ontario;
    "get owner of alaska" >:: territories_owner_test "1" alaska;
    "get owner of congo" >:: territories_owner_test "1" congo;
    "get owner of indonesia" >:: territories_owner_test "4" indonesia;
    "get owner of kamchatka" >:: territories_owner_test "2" kamchatka;
    "get owner of new guinea" >:: territories_owner_test "4" new_guinea;
    "get owner of great britain" >:: territories_owner_test "2" great_britain;
    "get owner of scandinavia" >:: territories_owner_test "1" scandinavia;
    "get owner of egypt" >:: territories_owner_test "3" egypt;
    "get owner of china" >:: territories_owner_test "1" china;
    "get owner of india" >:: territories_owner_test "3" india;
    "get owner of veneuzela" >:: territories_owner_test "5" venezuela;
    "get owner of argentina" >:: territories_owner_test "5" argentina;
  ]

(*------------------------ Player.get_territories_lst -----------------------*)
let get_territories_list_test out in1 _ =
  assert_equal ~printer:pp_country_list
    ~msg:
      ("function: get_territories_list\ninput: "
      ^ pp_country_option_array (Player.get_territories in1))
    out
    (Player.get_territories_lst in1)

let get_territories_list_tests =
  [
    "player 1 territories list"
    >:: get_territories_list_test
          [ alaska; congo; quebec; scandinavia; japan; china ]
          p1;
    "player 2 territories list"
    >:: get_territories_list_test
          [
            ontario;
            south_africa;
            northwest_territory;
            great_britain;
            kamchatka;
            ural;
            irkutsk;
          ]
          p2;
    "player 3 territories list"
    >:: get_territories_list_test [ egypt; india; madagascar; iceland ] p3;
    "player 4 territories list"
    >:: get_territories_list_test
          [ indonesia; new_guinea; western_australia; eastern_australia ]
          p4;
    "player 5 territories list"
    >:: get_territories_list_test [ venezuela; brazil; peru; argentina ] p5;
    "player 6 territories list" >:: get_territories_list_test [] p6;
  ]

(******************************************************************************)

let tests =
  "main test suite"
  >::: List.flatten
         [
           get_territories_list_tests;
           continent_get_name_tests;
           continent_get_value_tests;
           continent_to_string_tests;
           get_continent_number_tests;
           get_continent_value_tests;
           territories_owner_tests;
           territories_get_name_tests;
           territories_troops_value_tests;
           territories_get_continent_tests;
           territories_get_location_tests;
           territories_get_neighbours_tests;
           territories_string_neighbours_tests;
           player_get_territories_tests;
           player_get_name_tests;
           player_get_continent_bonus_tests;
           player_get_color_tests;
           player_get_territory_tests;
           player_num_territories_tests;
           player_territories_to_string_tests;
         ]

let _ = run_test_tt_main tests
