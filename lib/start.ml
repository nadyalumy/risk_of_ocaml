open Raylib

type t = {
  bg : Texture2D.t;
  button : Texture2D.t;
  button_hl : Texture2D.t;
}

let offset = 15
let start = ref None

let start_hb =
  Rectangle.create
    (681. +. float_of_int (2 * offset))
    (570. +. float_of_int (2 * offset))
    240. 100.

let initialize_start () =
  let bg_start_texture = load_texture "assets/start/StartBackground.png" in
  let start_button_texture = load_texture "assets/start/StartButton.png" in
  let start_button_highlight_texture =
    load_texture "assets/start/StartButtonHi.png"
  in
  start :=
    Some
      {
        bg = bg_start_texture;
        button = start_button_texture;
        button_hl = start_button_highlight_texture;
      }

let get_button_hl () =
  match !start with
  | None -> failwith "Failed to load in instructions textures"
  | Some s -> s.button_hl

let highlight_button_start mouse =
  let start_hl = (Option.get !start).button_hl in
  if check_collision_point_rec mouse start_hb then
    match is_mouse_button_pressed MouseButton.Left with
    | false ->
        draw_texture start_hl (681 - offset) (570 - offset)
          Constants.default_color
    | true -> Constants.game_state := MENU

let draw_start mouse =
  let start = Option.get !start in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro start.bg source dest origin 0. Constants.default_color;
  highlight_button_start mouse;
  draw_texture start.button 681 570 Constants.default_color
