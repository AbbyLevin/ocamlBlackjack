open Card
open Deck
open Player
open State
(*
  (** [play_game f] starts the adventure in file [f]. *)
  let play_game f =
  let init = init_game_state ["Abby"; "Austin"; "Brennan"] in 
  print_string "WINNER"

  (** [main ()] prompts for the game to play, then starts it. *)
  let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to BLACKJACK.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> failwith "Un"
  | interested -> if interested = "Y" then play_game interested else failwith "SDFKJ"

  (* Execute the game engine. *)
  let () = main ()

*)


(** [play_game level] starts the Blackjack game of difficulty [l]. *)
let prompt_name access =
  if access |> not then failwith "KSLDJF" else 
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
  | x -> let next_name = prompt_name in get_names (n-1) (next_name true () :: name_list)

(*for i = 1 to n do
  let next_name = prompt_name in 
  append_name next_name name_list;
  done *)

(** [play_easy_game] starts the easy Blackjack game. *)
let initialize_game initialize =
  if initialize |> not then failwith "SDLKJF" else
    ANSITerminal.(print_string [red]
                    "\n\nHow many players will there be?.\n");
  print_endline "\n";
  print_string  "> "; 
  match read_line () with
  | exception End_of_file -> failwith "GIVE NUM"
  | number_of_players -> (get_names (int_of_string(number_of_players)) []) |> init_game_state

let determine_big_winner init_state = 
  print_endline "\n";
  print_string "The winner is .... Congratulatory message"

let play_round here = 
  if here |> not then failwith "ROUNDDDD" else print_string "PLEASE"

let repeat_game quit here = 
  if here |> not then failwith "dkf" else 
    let quit_loop = ref false in
    while not quit do
      print_string "Have you had enough yet? (y/n) ";
      let str = read_line () in
      if str.[0] = 'y' then
        quit_loop := true else play_round true
    done;;

(** [play_game level] starts the Blackjack game of difficulty [l]. *)
let play_game initialize =
  print_endline "\n";
  print_string "Here are the rules... Could make this a nice feature
  about looking up the instructions. ";
  let init_state = initialize_game initialize in 
  repeat_game false true; 
  determine_big_winner init_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Would you like to start a game?\n";
  print_string  "> ";
  match read_line () with
  | interested -> if interested = "Y" then play_game true 
    else failwith "Handle a user not typing Y"

(* Execute the game engine. *)
let () = main ()


(*
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
  | interested -> if interested = "Y" then play_game interested else print_string interested

(* Execute the game engine. *)
let () = main () 

*)
