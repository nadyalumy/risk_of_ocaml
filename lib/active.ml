open Raylib

type t = {
  bg : Texture2D.t;
  phase : Texture2D.t;
  phase_hl : Texture2D.t;
}

let active = ref None
let starting = ref false (* check if the phase button has been pressed *)
let x_offset = 35
let y_offset = 20
let pb_x = 1372
let pb_y = 670

(* phase temp button hitbox *)
let phase_hb =
  Rectangle.create (float_of_int pb_x) (float_of_int pb_y) 150. 100.

(** [highlight_button_phase mouse] Outputs based on user interaction with the
    button. If the mouse hovers over, it changes texture. If it is clicked,
    changes the phase. *)
let highlight_button_phase mouse =
  if check_collision_point_rec mouse phase_hb then
    match is_mouse_button_pressed MouseButton.Left with
    | false ->
        draw_texture (Option.get !active).phase_hl pb_x pb_y Color.raywhite
    | true ->
        starting := true;
        Constants.game_active :=
          Some (Game.change_phase (Constants.get_game ()))

(** [initialize_active] Loads the textures for the active state.*)
let initialize_active () =
  let active_bg = load_texture "assets/active/map.png" in
  let phase = load_texture "assets/active/PhaseButton.png" in

  let phase_hl = load_texture "assets/active/PhaseButtonHi.png" in
  active := Some { bg = active_bg; phase; phase_hl }

(** [print_header] helper for draw_instructions.*)
let print_header s = draw_text s 1302 109 20 Color.black

(** [draw_instructions g] draws the text for the current phase of [game].*)
let draw_instructions (game : Game.t) =
  if !starting = true then
    match Game.get_phase game with
    | Deploy -> print_header "Deploy"
    | Attack -> print_header "Attack"
    | Fortify -> print_header "Fortify"
  else draw_text "Deploy" 1302 108 20 Color.black

(** [draw_territories_of_player p] draws the troop numbers text for every
    country that the player owns. *)
let draw_territories_of_player (player : Player.t) =
  let territories = Player.get_territories player in
  Array.iter
    (fun elem ->
      match elem with
      | None -> ()
      | Some territory ->
          let location = Territories.get_location territory in
          draw_text
            (string_of_int (Territories.get_troops territory))
            (fst location + x_offset)
            (snd location + y_offset)
            20 (Player.get_color player))
    territories

(** [draw_active mouse] Draws all the textures for the active phase onto the
    GUI.*)
let draw_active mouse =
  let a = Option.get !active in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro a.bg source dest origin 0. Constants.default_color;
  draw_texture a.phase pb_x pb_y Constants.default_color;

  let game = Constants.get_game () in

  let players = Game.get_players game in
  let _ = List.iter (fun player -> draw_territories_of_player player) players in
  ();

  (* draw current player *)
  let curr_player = Game.get_current_player game in
  let curr_player_name = Player.get_name curr_player in
  let curr_player_color = Player.get_color curr_player in
  let curr_player_string = "Current Player: " ^ curr_player_name in
  draw_text curr_player_string 350 840 40 curr_player_color;
  draw_instructions game;
  highlight_button_phase mouse
