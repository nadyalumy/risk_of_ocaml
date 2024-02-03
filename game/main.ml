open Lib
open Raylib

(* Game setup function *)
let setup () =
  init_window Constants.screen_width Constants.screen_height "risk_of_ocaml";
  set_target_fps Constants.screen_fps;
  Start.initialize_start ();
  Menu.initialize_menu ();
  Instructions.initialize_instructions ();
  Active.initialize_active ();
  ()

(* Game loop function *)
let rec loop () =
  match window_should_close () with
  | true -> close_window ()
  | false -> (
      match Constants.get_state () with
      | START ->
          begin_drawing ();
          clear_background Constants.default_color;
          let mouse = get_mouse_position () in
          Start.draw_start mouse;
          end_drawing ();
          loop ()
      | MENU ->
          begin_drawing ();
          clear_background Constants.default_color;
          let mouse = get_mouse_position () in
          Menu.draw_menu mouse;
          end_drawing ();
          loop ()
      | INSTRUCTIONS ->
          begin_drawing ();
          clear_background Constants.default_color;
          let mouse = get_mouse_position () in
          Instructions.draw_instructions mouse;
          end_drawing ();
          loop ()
      | ACTIVE ->
          begin_drawing ();
          clear_background Color.raywhite;
          let mouse = get_mouse_position () in
          Active.draw_active mouse;
          end_drawing ();
          loop ()
      | END ->
          Constants.game_state := START;
          loop ())

(* initializes game *)
let _ =
  setup ();
  loop ()
