open Card
open Deck
open Player
open State

(*
  (** [play_game level] starts the Blackjack game of difficulty [l]. *)
  let prompt_name =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome new player. What will your name be?.\n");
  print_endline "\n";
  print_string  "> ";
  match read_line with
  | exception End_of_file -> failwith("TYPE NAME")
  | name -> name

  let set_name_list name_list = 
  name_list

  let append_name next_name name_list = 
  set_name_list(next_name :: name_list)

  (** [play_game level] starts the Blackjack game of difficulty [l]. *)
  let rec get_names (n : int) (name_list : string list) =
  match n with 
  | 0 -> name_list
  | x -> let next_name = prompt_name in get_names (n-1) (next_name () :: name_list)

  (*for i = 1 to n do
  let next_name = prompt_name in 
  append_name next_name name_list;
  done *)


  (** [play_easy_game] starts the easy Blackjack game. *)
  let initialize_game =
  ANSITerminal.(print_string [red]
                  "\n\nHow many players will there be?.\n");
  print_endline "\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> failwith "GIVE NUM"
  | number_of_players -> (*init_game_state (get_names (int_of_string(number_of_players)) []) *) failwith "unimplemented"

  (** [play_game level] starts the Blackjack game of difficulty [l]. *)
  let play_game =
  initialize_game

  (** [main ()] prompts for the game to play, then starts it. *)
  let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Would you like to start a game?\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> failwith "FAILED"
  | interested -> if interested = "Y" then () else ()

  (* Execute the game engine. *)
  let () = main ()

*)

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> print_string file_name

(* Execute the game engine. *)
let () = main ()
