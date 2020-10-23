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

(** [player_turn] returns a player with their hand updated based on how 
    many times they hit *)
let rec player_turn player state =
  (** Check if cards are over 21 *)
  ANSITerminal.(print_string [red]
                  "\n\Press 'h' to hit or press 's' to stay.\n");
  print_string  "> ";
  print_hand (get_hand player);
  match read_line () with
  | "h" -> let new_player = hit player state in
    player_turn new_player state 
  | "s" -> player
  | _ -> player_turn player state

(** [play_turns] returns a player list with each player's hand updated based
    on how many times they decided to hit *)
let rec play_turns state acc = 
  match state.players with 
    [] -> acc
  | x :: xs -> let new_player = player_turn x state in 
    let new_state = {deck=state.deck; players=xs} in 
    play_turns new_state (new_player :: acc)

(** [deal_cards] returns a player list where each player's hand is updated 
    to contain two cards randomly selected from the deck *)
let rec deal_cards state acc = 
  match state.players with 
    [] -> acc 
  | x :: xs -> let new_player = initialize_hand x in 
    let new_state = {deck=state.deck; players=xs} in 
    deal_cards new_state (new_player :: acc)



(** [start_round] starts a new round of blackjack and returns the state once 
    the game is finished *)
let start_round state = 
  (* take cur_rotation and call the function that plays their turn *)
  let players_w_hands = deal_cards state [] in 
  let state_w_hands = {deck=state.deck; players=players_w_hands} in  
  (* place bets *)
  let players_after_turns = play_turns state_w_hands [] in 
  let new_state = {deck=state.deck; players = players_after_turns} in 
  new_state
