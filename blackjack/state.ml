open Player 
open Card 
open Deck 

type state = {deck: card list; players: player list}

(** [init_game_state player_names] returns a game state with as many players
    as are in [player_names] plus the house *)
let init_game_state player_names = 
  let deck = shuffle create_standard_deck in 
  let players = (List.map (fun x -> {name=x; hand=[]}) player_names) in 
  let players_house = players @ [{name="HOUSE"; hand=[]}] in 
  {deck=deck; players=players_house}


(** *)
let hit player state =
  failwith "hit"
(**   let top_card = List.fst deck in
      player.hand = top_card :: (get_hand player);
      List.remove deck top_card *)

(**  *)
let stay player state = 
  failwith "stay"

(** use while loop to hit until they decide to stay *)
let rec player_turn player state =
  ANSITerminal.(print_string [red]
                  "\n\Press 'h' to hit or press 's' to stay.\n");
  print_string  "> ";
  match read_line () with
  | "h" -> hit player state 
  | "s" -> stay player state
  | _ -> player_turn player state


let rec play_turns state = 
  match state.players with 
    [] -> state
  | x :: xs -> let new_state = player_turn x state in 
    play_turns new_state 

(**  *)
let start_round state = 
  (* take cur_rotation and call the function that plays their turn *)
  (* Deal cards *)
  (* place bets *)
  let new_state = play_turns state in 
  new_state
