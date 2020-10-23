open Card
open Deck
open Player
open State

(** [determine_big_winner final_state] determines who has won the most money
    at the end of [final_state]. *)
let determine_big_winner final_state = 
  print_endline "\n";
  print_string "The winner is .... Congratulatory message";
  print_endline "\n"

(** [play_round state here] carries out all of the functionality of playing a 
    round of Blackjack with associated game information [state]. [here] 
    is used to prevent this functionality from occuring when the 
    file is loaded. *)
let play_round state here = 
  if here |> not then failwith "Should never get here" 
  else print_string "TODO - implement play round"; state

(** [repeat_rounds state quit here] allows a user to play a round and 
    update [state] along the way as long as they don't make [quit] true. 
    [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
let rec repeat_rounds state quit here = 
  if here |> not then failwith "dkf" else 
    print_string "Have you had enough yet? (y/n) ";
  let str = read_line () in
  match str with 
  | "y" -> print_string "Thanks for playing! TODO - 
  implement a way for one player to stop, but the game doesn't. "
  | "n" ->  repeat_rounds (play_round state true) false true
  | other -> failwith "TODO - Please type a valid input."

(*
let repeat_rounds_fake here = 
  if here |> not then failwith "dkf" else 
    let quit_loop = ref false in
    while not !quit_loop do
      print_string "Have you had enough yet? (y/n) ";
      let str = read_line () in
      if str.[0] = 'y' then
        quit_loop := true else play_round true
    done;;

    *)

(** [prompt_name here] prompts a user to enter their name and handles
    their response. [here] is used to prevent this functionality from 
    occuring when the file is loaded. *)
let prompt_name here =
  if here |> not then failwith "KSLDJF" else 
    ANSITerminal.(print_string [red]
                    "\n\nWelcome new player. What will your name be?.\n");
  print_endline "\n";
  print_string  "> ";
  match read_line () with
  | name -> name

(** [get_names name_list n] returns [name_list] which has the names of the
    [n] players. *)
let rec get_names (name_list : string list) (n : int)  =
  match n with 
  | 0 -> name_list
  | x -> let next_name = prompt_name in 
    get_names (next_name true :: name_list) (n-1)

(** [initialize_game here] initializes the game by gathering the number of
    players and returning an initial game state with each of these players' 
    names. [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
let initialize_game here =
  if here |> not then failwith "Should never get here" else
    ANSITerminal.(print_string [red]
                    "\n\nHow many players will there be?.\n");
  ANSITerminal.(print_string [red] 
                  "Please enter a number between 1 and 100 inclusive.");
  print_endline "\n";
  print_string  "> "; 
  match read_line () with  
  | number_of_players -> let n = number_of_players |> int_of_string_opt in 
    if n = None || n < Some 0 || n > Some 100 
    then failwith "TODO - handle case where they don't give a valid num"
    else number_of_players |> int_of_string |> get_names [] |> init_game_state

(** [play_game here] starts the Blackjack game. [here] is used to prevent
    this functionality from occuring when the file is loaded. *)
let play_game here =
  print_endline "\n";
  print_string "Here are the rules... Could make this a nice feature
  about looking up the instructions. ";
  let init_state = initialize_game here in 
  let final_state = start_round init_state in 
  (*repeat_rounds_fake true; *)
  let final_round = repeat_rounds final_state false true in 
  determine_big_winner final_round

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Would you like to start a game?\n";
  print_string  "> ";
  match read_line () with
  | interested -> if interested = "Y" then play_game true 
    else failwith "TODO - Handle a user not typing Y"

(** Execute the game engine. *)
let () = main ()
