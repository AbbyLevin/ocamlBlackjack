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
  else start_round state

(** [repeat_rounds state quit here] allows a user to play a round and 
    update [state] along the way as long as they don't make [quit] true. 
    [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
let rec repeat_rounds state quit here = 
  if here |> not then failwith "dkf" else 
    print_string "\nWould you like to play another round? (y/n) ";
  let str = read_line () in
  match str with 
  | "n" -> print_string (determine_round_winners state);
    print_string "Thanks for playing! TODO - 
  implement a way for one player to stop, but the game doesn't. "
  | "y" ->  print_string (determine_round_winners state);
    repeat_rounds (play_round state true) false true
  | other -> print_string "\nPlease type a valid input.\n";
    repeat_rounds state quit here

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
let rec initialize_game here =
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
    then begin
      print_endline "\n";
      print_string "Invalid input. Try again.";
      initialize_game here
    end
    else number_of_players |> int_of_string |> get_names [] |> init_game_state

let rules_string = "Welcome to Blackjack! \nYour goal is to beat the dealer's hand without going over 21. \nFace cards are worth 10, aces are worth 1 or 11 (whichever is better for your hand). \nAll other cards are worth their value. \nEach player begins with two cards. \nThe players' cards are known to all, but only one of the dealer's cards is visible. \nTo hit (h) is to ask for another card. To stand (s) is end your turn without getting another card. \nIf the total value of your hand goes over 21, you bust, and the dealer wins. \n"

let rec rules here =
  print_endline "\n";
  print_string rules_string;
  print_endline "\n";
  ANSITerminal.(print_string [red] "Are you ready to play? (y/n)" );
  print_endline "\n";
  print_string  "> "; 
  match read_line () with 
  | "y" -> begin
      let init_state = initialize_game here in 
      let final_state = start_round init_state in 
      (*repeat_rounds_fake true; *)
      let final_round = repeat_rounds final_state false true in 
      determine_big_winner final_round
    end
  | _ -> begin
      print_endline "\n";
      print_string
        "That wasn't a yes, so here are the rules again. Try reading them this time. ";
      rules here
    end  

(** [play_game here] starts the Blackjack game. [here] is used to prevent
    this functionality from occuring when the file is loaded. *)
let rec play_game here =
  print_endline "\n";
  ANSITerminal.(print_string [red] "Would you like to see the rules? (y/n) ");
  print_endline "\n";
  print_string  "> "; 
  match read_line () with 
  | "y" -> rules here
  | "n" -> begin
      let init_state = initialize_game here in 
      let final_state = start_round init_state in 
      (*repeat_rounds_fake true; *)
      let final_round = repeat_rounds final_state false true in 
      determine_big_winner final_round
    end
  | _ -> begin
      print_endline "\n";
      print_string "Please type a valid input. ";
      play_game here
    end

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Blackjack.\n");
  ANSITerminal.(print_string [red] 
                  "Would you like to start a game? (press 'y' for yes)\n");
  print_string  "> ";
  match read_line () with
  | interested -> if interested = "y" then play_game true 
    else begin
      print_string "Why did you even start the game in the first place?\nBye";
      print_endline "\n"
    end

(** Execute the game engine. *)
let () = main ()
