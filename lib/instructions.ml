open Raylib

type t = {
  bg : Texture2D.t;
  continue : Texture2D.t;
  continue_hl : Texture2D.t;
  instructions : Texture2D.t list;
  mutable pg0 : bool;
  mutable pg1 : bool;
  mutable pg2 : bool;
  mutable pg3 : bool;
  mutable pg4 : bool;
}

let offset = 5
let instructions = ref None
let continue_b = Rectangle.create 755. 687. 100. 100.

let initialize_instructions () =
  let bg_instructions_texture =
    load_texture "assets/instructions/InstructionsBackground.png"
  in
  let instruction_button_texture = load_texture "assets/instructions/HB1.png" in
  let instruction_button_highlight =
    load_texture "assets/instructions/HB2.png"
  in
  let pg0 = load_texture "assets/instructions/pg0.png" in
  let pg1 = load_texture "assets/instructions/pg1.png" in
  let pg2 = load_texture "assets/instructions/pg2.png" in
  let pg3 = load_texture "assets/instructions/pg3.png" in
  let pg4 = load_texture "assets/instructions/pg4.png" in
  instructions :=
    Some
      {
        bg = bg_instructions_texture;
        continue = instruction_button_texture;
        continue_hl = instruction_button_highlight;
        instructions = [ pg0; pg1; pg2; pg3; pg4 ];
        pg0 = true;
        pg1 = false;
        pg2 = false;
        pg3 = false;
        pg4 = false;
      }

let get_button_hl () =
  match !instructions with
  | None -> failwith "Failed to load in start textures"
  | Some s -> s.continue

let draw_pg pg = draw_texture pg (295 - offset) 194 Constants.default_color

let highlight_button_instructions mouse =
  let instructions = Option.get !instructions in
  let button_hl = instructions.continue_hl in
  if check_collision_point_rec mouse continue_b then
    match is_mouse_button_pressed MouseButton.Left with
    | false -> draw_texture button_hl (750 + offset) 687 Constants.default_color
    | true when instructions.pg0 ->
        instructions.pg0 <- false;
        instructions.pg1 <- true;
        wait_time 0.1
    | true when instructions.pg1 ->
        instructions.pg1 <- false;
        instructions.pg2 <- true;
        wait_time 0.1
    | true when instructions.pg2 ->
        instructions.pg2 <- false;
        instructions.pg3 <- true;
        wait_time 0.1
    | true when instructions.pg3 ->
        instructions.pg3 <- false;
        instructions.pg4 <- true;
        wait_time 0.1
    | true -> Constants.game_state := ACTIVE

let draw_instructions mouse =
  let instructions = Option.get !instructions in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  let pg0 = List.nth instructions.instructions 0 in
  let pg1 = List.nth instructions.instructions 1 in
  let pg2 = List.nth instructions.instructions 2 in
  let pg3 = List.nth instructions.instructions 3 in
  let pg4 = List.nth instructions.instructions 4 in
  draw_texture_pro instructions.bg source dest origin 0. Constants.default_color;
  draw_texture instructions.continue (750 + offset) 687 Constants.default_color;
  highlight_button_instructions mouse;
  if instructions.pg0 then draw_pg pg0
  else if instructions.pg1 then draw_pg pg1
  else if instructions.pg2 then draw_pg pg2
  else if instructions.pg3 then draw_pg pg3
  else if instructions.pg4 then draw_pg pg4
  else ()
