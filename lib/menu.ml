open Raylib
open Raygui

type t = {
  bg : Texture2D.t;
  two_pb : Texture2D.t;
  two_pb_hl : Texture2D.t;
  three_pb : Texture2D.t;
  three_pb_hl : Texture2D.t;
  four_pb : Texture2D.t;
  four_pb_hl : Texture2D.t;
  five_pb : Texture2D.t;
  five_pb_hl : Texture2D.t;
  six_pb : Texture2D.t;
  six_pb_hl : Texture2D.t;
  name_sel : Texture2D.t;
}

let menu = ref None
let two_pb_hb = Rectangle.create 304. 348. 100. 100.
let three_pb_hb = Rectangle.create 527. 348. 100. 100.
let four_pb_hb = Rectangle.create 750. 348. 100. 100.
let five_pb_hb = Rectangle.create 971. 348. 100. 100.
let six_pb_hb = Rectangle.create 1194. 348. 100. 100.
let player_names = ref []
let num_players = ref 7
let tb_edit = ref false
let tb_text = ref ""
let tb = Rectangle.create 550. 600. 500. 80.
let show_tb = ref false

let is_valid_char a =
  match a with
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '1' .. '9' -> true
  | '.' | ',' -> true
  | ' ' -> true
  | _ -> false

(** [string_compare s1 s2] String comparison for s1 and s2 because
    Stdlib.compare and String.compare did not work for Raylib text box outpus.*)
let rec string_compare s1 s2 =
  let s1 = String.lowercase_ascii s1 in
  let s2 = String.lowercase_ascii s2 in
  if String.length s1 <> String.length s2 then false
  else if String.length s1 = 0 && String.length s2 = 0 then true
  else if
    String.get s1 0 = String.get s2 0
    && is_valid_char (String.get s1 0)
    && is_valid_char (String.get s2 0)
  then
    string_compare
      (String.sub s1 1 (String.length s1 - 1))
      (String.sub s1 1 (String.length s1 - 1))
  else false

(** Raylib text box is broken and any string that is returned has a bunch of
    non-existent characters appended to it. [fix_string s] returns a string
    composed of the valid characters of [s]. Requires: all valid characters come
    before the first invalid character*)
let fix_string s =
  let count = ref 0 in
  for i = 0 to String.length s - 1 do
    if is_valid_char (String.get s i) then incr count
  done;
  String.sub s 0 !count

(** [get_text] Returns any user input in the GUI text box if the user presses
    ENTER.*)
let get_text () =
  match is_key_pressed Enter with
  | true ->
      let temp = !tb_text in
      tb_text := "";
      Some (fix_string temp)
  | false -> None

(** [try_get_player_names n] Takes in user input from the GUI text box and adds
    the string to [player_names] which is a list of player names. If the name is
    already in the list then it does nothing.*)
let try_get_player_names n =
  if List.length !player_names < n then
    match get_text () with
    | Some name ->
        print_endline ("Name: " ^ name);
        if
          List.exists (fun p -> String.trim p = String.trim name) !player_names
          = false
        then player_names := name :: !player_names
        else print_endline "Name already chosen."
    | None -> ()
  else ()

(** [initialize_menu] loads the textures of the menu.*)
let initialize_menu () =
  let bg_menu_texture = load_texture "assets/menu/MenuBackground.png" in
  let two_pb_texture = load_texture "assets/menu/2PB.png" in
  let two_pb_highlight_texture = load_texture "assets/menu/2PBHighlight.png" in

  let three_pb_texture = load_texture "assets/menu/3PB.png" in
  let three_pb_highlight_texture =
    load_texture "assets/menu/3PBHighlight.png"
  in

  let four_pb_texture = load_texture "assets/menu/4PB.png" in
  let four_pb_highlight_texture = load_texture "assets/menu/4PBHighlight.png" in

  let five_pb_texture = load_texture "assets/menu/5PB.png" in
  let five_pb_highlight_texture = load_texture "assets/menu/5PBHighlight.png" in

  (* active state *)
  let six_pb_texture = load_texture "assets/menu/6PB.png" in
  let six_pb_highlight_texture = load_texture "assets/menu/6PBHighlight.png" in
  let choose_name_txt = load_texture "assets/menu/ChooseName.png" in
  menu :=
    Some
      {
        bg = bg_menu_texture;
        two_pb = two_pb_texture;
        two_pb_hl = two_pb_highlight_texture;
        three_pb = three_pb_texture;
        three_pb_hl = three_pb_highlight_texture;
        four_pb = four_pb_texture;
        four_pb_hl = four_pb_highlight_texture;
        five_pb = five_pb_texture;
        five_pb_hl = five_pb_highlight_texture;
        six_pb = six_pb_texture;
        six_pb_hl = six_pb_highlight_texture;
        name_sel = choose_name_txt;
      }

(** [highlight_button_menu mouse] Outputs based on user interaction with the
    button. If the mouse hovers over, it changes texture. If it is clicked,
    switches to "choose name" state. *)
let highlight_button_menu mouse hitbox highlight (x, y) n =
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_pressed MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        show_tb := true;
        player_names := [];
        num_players := n

(** [draw_menu mouse] Draws the textures of the menu state.*)
let draw_menu mouse =
  let menu = Option.get !menu in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro menu.bg source dest origin 0. Constants.default_color;
  if not !show_tb then (
    draw_texture menu.two_pb 304 348 Constants.default_color;
    draw_texture menu.three_pb 527 348 Constants.default_color;
    draw_texture menu.four_pb 750 348 Constants.default_color;
    draw_texture menu.five_pb 971 348 Constants.default_color;
    draw_texture menu.six_pb 1194 348 Constants.default_color;
    highlight_button_menu mouse two_pb_hb menu.two_pb_hl (294, 343) 2;
    (* 2 *)
    highlight_button_menu mouse three_pb_hb menu.three_pb_hl (517, 343) 3;
    (* 3 *)
    highlight_button_menu mouse four_pb_hb menu.four_pb_hl (740, 343) 4;
    (* 4 *)
    highlight_button_menu mouse five_pb_hb menu.five_pb_hl (961, 343) 5;
    (* 5 *)
    highlight_button_menu mouse six_pb_hb menu.six_pb_hl (1184, 343) 6
    (* 6 *))
  else (
    (if !show_tb then draw_texture menu.name_sel 643 553 Constants.default_color;
     match text_box tb !tb_text !tb_edit with
     (* vl is the text inside the textbox *)
     | vl, true ->
         tb_edit := not !tb_edit;
         tb_text := vl;
         try_get_player_names !num_players
     | vl, false ->
         if "" <> vl then tb_text := vl else ();

         set_style (TextBox `Text_alignment) TextAlignment.(to_int Left);
         set_style (TextBox `Color_selected_bg) 200);

    let plst = !player_names in
    let np = !num_players in
    if List.length plst = np then (
      Constants.game_active := Some (Game.init plst np);
      Constants.game_state := INSTRUCTIONS)
    else ())
