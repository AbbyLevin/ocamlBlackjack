open Player 
open Card 
open Deck 

type state = {deck: card list; players: player list}

(** [init_game_state player_names] returns a game state with as many players
    as are in [player_names] plus the house *)
let init_game_state player_names = 
  let deck = shuffle create_standard_deck in 
  let players = (List.map (fun x -> {name=x; hand=[]}) player_names) in 
  let players_house = [{name="HOUSE"; hand=[]}] @ players in 
  {deck=deck; players=players_house}

let quit state = 
  failwith "Unimplimented"
(* ANSITerminal.(print_string [blue] "Thanks for playing!") *)

(** [player_turn] returns a player with their hand updated based on how 
    many times they hit *)
let rec player_turn player state =
  ANSITerminal.(print_string [green] ("\n" ^ player.name ^ "'s turn: \n"));
  if (get_sum player > 21) then begin
    print_hand player.hand;
    print_string "\n";
    print_string (player.name ^ " has busted. Haha loser.\n");
    player
  end
  else begin
    ANSITerminal.(print_string [red]
                    "\nPress 'h' to hit or press 's' to stay.\n");
    print_string  "> ";
    print_hand (get_hand player);
    if player.name = "HOUSE" 
    then let card_sum = sum_cards (get_hand player) in 
      begin 
        match card_sum with 
        | s when s < 17 -> player_turn (house_turn player state) state
        | s when s <= 21 -> player 
        | _ -> player_turn (house_turn player state) state
      end
    else 
      match read_line () with
      | "h" -> let new_player = hit player state in
        player_turn new_player state 
      | "s" -> player
      | "quit" -> quit state; player
      | _ -> player_turn player state
  end

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
  let new_state = {deck = state.deck; players = players_after_turns} in 
  new_state

(** [winners_list lst] returns a list of all of the winners of a round 
    represented by [lst] and their scores. *)
let winners_list (lst : (Player.player * int) list) = 
  let max_vote_total = (snd (List.hd lst)) in
  List.filter (fun x -> snd(x) = max_vote_total) lst  

(** [output_multiple_winners winners_list] returns a string that contains the 
    names of all of the winners separated by a semicolon. *)
let rec output_multiple_winners winners_list score = 
  match winners_list with
  | [] -> "are tied with a score of " ^ score ^ 
          (* (List.hd winners_list |> snd |> string_of_int) *)
          ", so they all won this round. Congrats!\n\n"
  | h :: t -> if t = [] 
    then "and " ^ (fst h).name ^ " " ^ output_multiple_winners t score
    else if List.length t = 1 
    then (fst h).name ^ " " ^ output_multiple_winners t score
    else (fst h).name ^ ", " ^ output_multiple_winners t score

(** [get_winner player_sums] determines the winner(s) of a list of players 
    and their scores [player_sums]. *)
let get_winner (player_sums : (Player.player * int) list) : string =  
  let winners = winners_list player_sums in
  if List.length winners = 1 then "\n" ^ (fst (List.hd winners)).name 
                                  ^ " won this round with a score of " 
                                  ^ (List.hd winners |> snd |> string_of_int) 
                                  ^ ". Congrats!\n\n"
  else if List.length winners = 0 then "The House won this round.\n\n" 
  else output_multiple_winners winners 
      (List.hd winners |> snd |> string_of_int)

(** [get_player_sums acc players] returns a dictionary [acc] where the key 
    is each player's name in [players] and the associated value is their 
    score at the end of the round. *)
let rec get_player_sums acc (players : Player.player list) = 
  match players with 
  | [] -> acc
  | h :: t -> get_player_sums ((h, (get_sum h)) :: acc) t

(** [compare_players x y] compares [x] and [y] based on their scores. *)
let compare_players x y = 
  if snd x > snd y then 1 
  else if snd x < snd y then -1 
  else 0

(** [determine_round_winner determines the winner(s) of round [curr]. *)
let determine_round_winners curr = 
  let player_sums = get_player_sums [] curr.players in
  let not_elim = List.filter (fun x -> snd(x) <= 21) player_sums in  
  let sorted = List.sort compare_players not_elim |> List.rev in 
  let round_winners = get_winner sorted in 
  round_winners