module Y = Yojson.Basic.Util

type player = Player.t
type players = player list
type territory = Territories.t
type territories = territory array

exception InvalidDice
exception InvalidOwnedCountry
exception InvalidAttackCountry

(** AF: The record {players: _ ; current_player: _  ; current_phase: _ ; 
  territories: _ ; troops_to_place: _ ; game_over: _ ;} represents a game. 
  RI: Players list must never be empty, current player must always be a player 
  in the player list, and troops to place must never be below 0. *)

type phase =
  | Deploy
  | Attack
  | Fortify

type t = {
  mutable players : players;
  mutable current_player : player;
  current_phase : phase;
  territories : territories;
  troops_to_place : int;
  mutable game_over : bool;
  mutable deploy_troops : int;
}

(* Holds the current map *)
let json = "data/countries.json"
let path = Yojson.Basic.from_file json

(**Array of colors that have not been taken by a player. If None, then the color
   has been taken. RI: All Some values are to the left of the array*)
let colors_left : Raylib.Color.t option array =
  [|
    Some Raylib.Color.gray;
    Some Raylib.Color.blue;
    Some Raylib.Color.green;
    Some Raylib.Color.red;
    Some Raylib.Color.yellow;
    Some Raylib.Color.purple;
  |]

(*************************** Helpers **********************************)
let rec pp_lst pp_elt lst =
  match lst with
  | [ h ] -> pp_elt h
  | h :: t -> pp_elt h ^ ", " ^ pp_lst pp_elt t
  | [] -> ""

let pp_territory_list t = pp_lst (fun s -> s) t
let get_player s lst = List.find (fun p -> Player.get_name p = s) lst
let get_players game = game.players

(** [Catch_error f msg] Runs the function f which takes in an input
    (readline()), and reruns the function and prints a message if there is and
    error. *)
let rec catch_error f msg =
  try f (read_line ())
  with _ ->
    print_endline (msg ^ "\n");
    catch_error f msg

(** [terlst_to_string lst] Given a list of territories [lst], returns the string
    version of the territories*)
let terlst_to_string (lst : Territories.t list) : string =
  List.fold_left
    (fun acc x ->
      acc ^ Territories.get_name x ^ ": "
      ^ string_of_int (Territories.get_troops x)
      ^ "\n")
    "" lst

(***************************Sampling helpers***********************************)

(**Returns the number of non-None elements in an option array that has shifted
   all None elements right. Requires: All Some values are left of all None
   values. *)
let arr_size arr =
  if arr.(Array.length arr - 1) <> None then Array.length arr
  else
    let counter = ref 0 in
    while arr.(!counter) <> None && !counter < Array.length arr - 1 do
      incr counter
    done;
    !counter

(** Samples from an option array and removes that element from the array by
    switching it to option. Requires: All Some values are left of all None
    values. *)
let sample arr size =
  if size <= 0 then None
  else
    let i = Random.int size in
    let v = arr.(i) in
    (* swap *)
    arr.(i) <- arr.(size - 1);
    arr.(size - 1) <- None;
    v

(*************************** Rep_ok **********************************)

let check_territory_rep g =
  let t =
    List.fold_left
      (fun acc (p : player) -> Player.get_territories_lst p @ acc)
      [] g.players
  in
  List.length (List.sort_uniq Stdlib.compare t) = 42

(** Checks representation invariants during gameplay.*)
let rep_ok (g : t) : t =
  let chk1 = g.troops_to_place >= 0 in
  let chk2 = List.exists (( == ) g.current_player) g.players in
  let chk3 = List.length g.players <> 0 in
  let chk4 = check_territory_rep g in
  if chk1 && chk2 && chk3 && chk4 then g else failwith "Rep Invariant Violated"

(*****************************************************************************)
(*******************************End game functions****************************)

(* TODO: change game over to END phase in constants *)
let check_game_over g =
  if List.length g.players = 1 then g.game_over <- true else ()

let remove_player g p =
  let plst = g.players in
  let plst' = List.filter (fun p1 -> p1 <> p) plst in
  g.players <- plst'

let check_player_lost g p =
  if List.length (Player.get_territories_lst p) = 0 then remove_player g p
  else ()

let get_game_over g = g.game_over

(****************************************************************************)
(*************************************************************************)

(*************************** Initialization**********************************)

(* Given a int [n], initializes players and returns a list of the players
   initialized. USER CHANGES NAME ECT ECT *)

let rec init_players_helper (n : int) (plst : string list) : players =
  match (n, plst) with
  | 0, _ -> []
  | a, h :: t -> begin
      match sample colors_left (arr_size colors_left) with
      | None -> failwith "Too many players"
      | Some c -> Player.init h c :: init_players_helper (a - 1) t
    end
  | _, [] -> failwith "violates rep inv"

let init_players (plst : string list) (n : int) : players =
  init_players_helper n plst
(* match n with | 0 -> [] | a -> ( print_endline ("\nSelect name for player " ^
   string_of_int a); let input = read_line () in print_endline ("Player " ^
   string_of_int a ^ " Succesfully Initialized"); match sample colors_left
   (arr_size colors_left) with | None -> failwith "Too many players" | Some c ->
   Player.init input c :: init_players (a - 1)) *)

(**Inihtializes the Territories in a game *)
let init_territories = path |> Map.create_map |> Map.get_territories

(** [get_territory_game s] returns the territory equivalent of the string [s]/
    Not case sensitive. *)
let get_territory_game (s : string) : Territories.t =
  let ter =
    Array.find_opt
      (fun t ->
        String.lowercase_ascii (Territories.get_name t)
        = String.lowercase_ascii s)
      init_territories
  in
  match ter with
  | None -> failwith "impossible"
  | Some c -> c

let to_option_array (arr : 'a array) : 'a option array =
  Array.map (fun v -> Some v) arr

(** Assigns Territories to each player randomly.*)
let assign_Territories plst =
  let _ = Random.self_init () in
  let temp = to_option_array (Array.copy init_territories) in
  for i = 0 to arr_size temp - 1 do
    let player_ind = i mod List.length plst in
    let player = List.nth plst player_ind in
    let c = sample temp (arr_size temp) in
    match c with
    | None -> failwith "impossible"
    | Some c1 -> Player.add_territory player c1
  done;
  plst

let num_troops_per_player n =
  match n with
  | 2 -> 40
  | 3 -> 35
  | 4 -> 30
  | 5 -> 25
  | 6 -> 20
  | _ -> failwith "Invalid number of players"

let assign_troops_helper t p =
  let countries = Player.get_territories p in
  for i = 0 to t - 1 do
    let index = i mod arr_size countries in
    match countries.(index) with
    | None -> failwith "impossible"
    | Some c -> Territories.add_value 1 c
  done;
  p

let assign_troops n plst = List.map (assign_troops_helper n) plst

(** Initializes game given a number of players *)
let init (nplst : string list) (numPlayers : int) =
  let plist =
    assign_troops
      (num_troops_per_player numPlayers)
      (assign_Territories (init_players nplst numPlayers))
  in
  {
    players = plist;
    current_player = List.hd plist;
    current_phase = Deploy;
    territories = init_territories;
    troops_to_place = 0;
    game_over = false;
    deploy_troops = 0;
  }

(****************************************************************************)

(******************************* ATTACK****************************************)

let rec roll_dice (n : int) : int list =
  match n with
  | 0 -> []
  | _ -> (Random.int 6 + 1) :: roll_dice (n - 1)

(** [territories_to_attack t] Given a territory [t], returns a list of the
    territories that can be attacked. Attackable territories cannot be owned by
    the same player who owns the territory)*)
let territories_to_attack t =
  let neighbours = Territories.get_neighbours t in
  List.filter
    (fun n ->
      let t2 = get_territory_game n in
      Territories.get_owner t2 <> Territories.get_owner t)
    neighbours

(** [can_attack_territories p]Given a player [p], returns a list of the
    territories that can attack (have more than one troop and its neighbours are
    not owned by the given player [p])*)
let can_attack_territories p =
  let t_lst = Player.get_territories_lst p in
  List.filter
    (fun t ->
      Territories.get_troops t > 1
      && if territories_to_attack t = [] then false else true)
    t_lst

let attacking atk atk_player def def_player game =
  print_endline
    (Territories.get_name atk ^ " attacks " ^ Territories.get_name def);

  let atk_troops = Territories.get_troops atk - 1 in
  let def_troops = Territories.get_troops def in
  let _ =
    print_endline
      ("Choose how many troops to attack with (max: "
      ^ string_of_int (Int.min 3 atk_troops)
      ^ ", min: 1)")
  in
  let atk_dice =
    catch_error
      (fun x ->
        match int_of_string x with
        | n when n > 3 -> raise InvalidDice
        | n when n = 3 ->
            if atk_troops >= 3 then roll_dice 3 else raise InvalidDice
        | n when n = 2 ->
            if atk_troops >= 2 then roll_dice n else raise InvalidDice
        | n when n = 1 ->
            if atk_troops >= 1 then roll_dice n else raise InvalidDice
        | _ -> raise InvalidDice)
      "Invalid Input"
  in
  let _ = print_endline ("attack rolls: " ^ pp_lst string_of_int atk_dice) in
  let def_dice =
    match def_troops with
    | n when n > 1 -> roll_dice 2
    | n when n > 0 -> roll_dice n
    | _ -> failwith "Violates rep_inv"
  in
  let _ = print_endline ("defense rolls: " ^ pp_lst string_of_int def_dice) in
  let rec cmp (a : int list) (d : int list) : unit =
    match
      ( List.rev (List.sort Stdlib.compare a),
        List.rev (List.sort Stdlib.compare d) )
    with
    | [], [] -> ()
    | _, [] ->
        let d_t = Territories.get_troops def in
        if d_t <= 0 then (
          print_endline "Attack wins";
          print_endline
            ("Choose the number of troops to move over, min: 1, max: "
            ^ string_of_int (Territories.get_troops atk - 1));
          let n =
            catch_error
              (fun x ->
                if 1 <= int_of_string x then int_of_string x else failwith "")
              "Invalid Input"
          in
          Territories.add_value n def;
          Territories.subtract_value n atk;
          Player.add_territory atk_player def;
          Player.remove_territory def_player def;
          check_player_lost game def_player;
          check_game_over game;
          if game.game_over then
            print_endline
              ("Player " ^ Player.get_name atk_player ^ " won the game!")
          else ())
        else ()
    | h :: t, h2 :: t2 ->
        if h > h2 then (
          let _ = print_endline "Defense lost one troop" in
          Territories.subtract_value 1 def;
          let d_t = Territories.get_troops def in
          if d_t <= 0 then (
            print_endline "Attack wins";
            print_endline
              ("Choose the number of troops to move over, min: 1, max: "
              ^ string_of_int (Territories.get_troops atk - 1));
            let n =
              catch_error
                (fun x ->
                  if 1 <= int_of_string x then int_of_string x else failwith "")
                "Invalid Input"
            in
            Territories.add_value n def;
            Territories.subtract_value n atk;
            Player.add_territory atk_player def;
            Player.remove_territory def_player def;
            check_player_lost game def_player;
            check_game_over game;
            if game.game_over then
              print_endline
                ("Player " ^ Player.get_name atk_player ^ " won the game!")
            else ())
          else cmp t t2)
        else
          let _ = print_endline "Attack lost one troop" in
          Territories.subtract_value 1 atk;
          cmp t t2
    | [], _ ->
        print_endline "Attack lost";
        ()
  in
  cmp atk_dice def_dice

let attack (game : t) =
  let _ = print_endline "Choose which territory to attack with:" in
  let atk_opt = can_attack_territories game.current_player in
  print_endline (terlst_to_string atk_opt);
  let atk_ter =
    catch_error
      (fun x ->
        if
          List.exists
            (fun t ->
              String.lowercase_ascii (Territories.get_name t)
              = String.lowercase_ascii
                  (Territories.get_name
                     (Player.get_territory game.current_player x)))
            atk_opt
        then Player.get_territory game.current_player x
        else raise InvalidOwnedCountry)
      "Invalid input"
  in
  let atk_player = get_player (Territories.get_owner atk_ter) game.players in
  print_endline "\nChoose which territory to attack: ";
  let def_options = territories_to_attack atk_ter in
  print_endline (pp_territory_list def_options);
  let def_ter =
    catch_error
      (fun x ->
        if
          List.exists
            (fun s -> String.lowercase_ascii s = String.lowercase_ascii x)
            def_options
        then get_territory_game x
        else raise InvalidAttackCountry)
      "Invalid input"
  in
  let def_player = get_player (Territories.get_owner def_ter) game.players in
  attacking atk_ter atk_player def_ter def_player game

(****************************************************************************)

(******************************Fortify***********************************)
let owned_neighbours t =
  let neighbours = Territories.get_neighbours t in
  List.filter
    (fun n ->
      let t2 = get_territory_game n in
      Territories.get_owner t2 = Territories.get_owner t)
    neighbours

(** [can_fortify_territories p] Given a player [p], returns a list of the
    territories that can be fortified. These territories are territories not
    landlocked by others player's countries. [p])*)
let can_fortify_territories p =
  let t_lst = Player.get_territories_lst p in
  List.filter
    (fun t ->
      Territories.get_troops t > 1
      && if owned_neighbours t = [] then false else true)
    t_lst

let rec fortify_territories tlst acc (visited : string list) =
  let t = List.hd tlst in
  let visited' = t :: visited in
  let n = owned_neighbours (get_territory_game t) in
  let new_t =
    List.filter
      (fun t1 -> List.exists (fun t2 -> t1 = t2) visited = false)
      (n @ tlst)
  in
  let acc' = List.sort_uniq Stdlib.compare (new_t @ acc) in
  match new_t with
  | [] -> acc'
  | _ ->
      let tlst' = new_t in
      fortify_territories tlst' acc' visited'

let fortify p =
  print_endline "\n\n\nDo you want to fortify? (Yes/No).";
  let err =
    catch_error
      (fun x ->
        match x with
        | "yes" -> x
        | "no" -> x
        | "Yes" -> x
        | "No" -> x
        | _ -> failwith "")
      "Invalid Input"
  in
  if String.lowercase_ascii err = "no" then ()
  else (
    print_endline "Select a territory to move troops from: ";
    let fort_ter = can_fortify_territories p in
    print_endline (terlst_to_string fort_ter);
    let t1 =
      catch_error
        (fun x ->
          if
            List.exists
              (fun y ->
                String.lowercase_ascii (Territories.get_name y)
                = String.lowercase_ascii
                    (Territories.get_name (get_territory_game x)))
              fort_ter
          then get_territory_game x
          else raise InvalidOwnedCountry)
        "Invalid Input"
    in
    let move_trps = Territories.get_troops t1 - 1 in
    let _ =
      print_endline
        ("Select the number of troops to move: max " ^ string_of_int move_trps)
    in
    let n =
      catch_error
        (fun x ->
          let num = int_of_string x in
          if num >= 1 && num <= move_trps then num else failwith "")
        ("Number must be between 1 and " ^ string_of_int move_trps)
    in
    let _ = print_endline "Choose the territory to move troops to " in
    let tlst = fortify_territories [ Territories.get_name t1 ] [] [] in
    print_endline (pp_lst (fun s -> s) tlst);
    let t2 =
      catch_error
        (fun x ->
          if
            List.exists
              (fun y ->
                String.lowercase_ascii y
                = String.lowercase_ascii
                    (Territories.get_name (get_territory_game x)))
              tlst
          then get_territory_game x
          else raise InvalidOwnedCountry)
        "Invalid Input"
    in
    Territories.add_value n t2;
    Territories.subtract_value n t1)

(****************************************************************************)

(******************************Deploy**********************************)

(** [get_troops p] given a player [p], return the amount of troops they are able
    to deploy*)
let get_troops (p : player) : int =
  let n =
    int_of_float (ceil (float_of_int (Player.num_territories p) /. 3.))
    + Player.get_continent_bonus p
  in
  if n < 3 then 3 else n

(** [Deploy g] given a game [g], tell the player to deploy their troops in their
    territories and deploy those troops int the cooresponding territories. Only
    return when the player has finished deploying their troops. *)
let deploy g =
  let new_troops = g.deploy_troops in
  print_endline
    ("\n" ^ string_of_int new_troops
   ^ " troops have been drafted. Select the country you want to deploy in: ");
  let _ = print_endline (Player.territories_to_string g.current_player) in
  let input =
    catch_error (Player.get_territory g.current_player) "Invalid Territory Name"
  in
  let should_loop = ref true in
  let troops_chosen = ref 0 in
  while !should_loop do
    print_endline
      ("Select the number of troops you wish to deploy: ("
     ^ string_of_int new_troops ^ " Troops avaliable)");
    troops_chosen :=
      catch_error
        (fun x ->
          if int_of_string x <= new_troops && int_of_string x > 0 then
            int_of_string x
          else failwith "")
        "Invalid Input";
    if !troops_chosen <= new_troops then should_loop := false
    else should_loop := true
  done;
  let _ = Territories.add_value !troops_chosen input in
  g.deploy_troops <- new_troops - !troops_chosen

(****************************************************************************)

(**********************Phase change helpers************************************)
let phase_to_string (phase : phase) : string =
  match phase with
  | Deploy -> "deploy"
  | Attack -> "attack"
  | Fortify -> "fortify"

(** [Change_phase_helper p g] given phase [p] and a game [g] return the game
    with the phase changed to phase [p]*)
let change_phase_helper p g =
  rep_ok
    {
      players = g.players;
      current_player = g.current_player;
      current_phase = p;
      territories = g.territories;
      troops_to_place = g.troops_to_place;
      game_over = g.game_over;
      deploy_troops = g.deploy_troops;
    }

(** Given a game, return the current player *)
let get_current_player game = game.current_player

(* Return the next player given a list of players INEFFICIENT: IMPLEMENT
   BETTER*)
let rec next_player_helper plist cp original =
  match plist with
  | [] -> failwith ""
  | [ _ ] -> List.hd original
  | h1 :: h2 :: t ->
      if h1 = cp then h2 else next_player_helper (h2 :: t) cp original

(** given a game, return the next player*)
let next_player game =
  next_player_helper game.players game.current_player game.players

(** Given a game, return the phase*)
let get_phase game = game.current_phase

(** Given a game, return the Territories*)
let get_territories game = game.territories

let get_remaining_troops game = game.deploy_troops
(**********************************************************)

(********************** Phase Change **************************************)

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (game : t) : t =
  print_endline
    ("\n\n\n\n\n\n\n\n\n\nIt is "
    ^ Player.get_name game.current_player
    ^ "'s turn");
  print_endline ("The current phase is " ^ phase_to_string game.current_phase);
  match game.current_phase with
  | Deploy ->
      if game.deploy_troops = 0 then (
        game.deploy_troops <- get_troops game.current_player;
        deploy game)
      else deploy game;
      if game.deploy_troops <= 0 then change_phase_helper Attack game
      else change_phase_helper Deploy game
  | Attack ->
      print_endline "\n\n\nDo you want to attack? (Yes/No).";
      let err =
        catch_error
          (fun x ->
            match x with
            | "yes" -> x
            | "no" -> x
            | "Yes" -> x
            | "No" -> x
            | _ -> failwith "")
          "Invalid Input"
      in
      if String.lowercase_ascii err = "no" then change_phase_helper Fortify game
      else (
        attack game;
        change_phase_helper Attack game)
  | Fortify ->
      fortify game.current_player;
      print_endline
        ("Player " ^ Player.get_name game.current_player ^ "'s turn is over.");
      game.current_player <- next_player game;
      change_phase_helper Deploy game
