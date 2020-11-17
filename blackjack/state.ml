open Player 
open Card 
open Deck 

type state = {players: player list; house: player}

(** [init_game_state player_names] returns a game state with as many players
    as are in [player_names] plus the house *)
let init_game_state player_names start_bal= 
  (* let deck = shuffle create_standard_deck in  *)
  let players = (List.map 
                   (fun x -> 
                      {name=x; hand=[]; balance=start_bal; current_bet=0}) 
                   player_names) in 
  let house = {name="HOUSE"; hand=[]; balance=max_int; current_bet=0} in 
  {players=players; house=house}

(** [player_turn] returns a player with their hand updated based on how 
    many times they hit *)
let rec player_turn player state =
  ANSITerminal.(print_string [green] ("\n\n" ^ player.name ^ "'s turn: \n"));
  if player.name <> "HOUSE" then begin
    print_string ("Current balance: " ^ string_of_int player.balance ^ "\n");
    print_string 
      ("This round's bet: " ^ string_of_int player.current_bet ^ "\n"); 
  end 
  else ();
  if (get_sum player > 21) then begin
    print_hand player.hand;
    print_string "\n\n";
    print_string (player.name ^ " has busted. Haha loser.\n");
    player
  end
  else begin
    print_hand (get_hand player);
    if player.name = "HOUSE" 
    then let card_sum = sum_cards (get_hand player) in 
      begin 
        match card_sum with 
        | s when s < 17 -> player_turn (house_turn player state) state
        | s when s <= 21 -> player 
        | _ -> player_turn (house_turn player state) state
      end
    else begin
      ANSITerminal.(print_string [red]
                      "Press 'h' to hit or press 's' to stay.\n");
      print_string  "> ";
      match read_line () with
      | "h" -> let new_player = hit player state in
        player_turn new_player state 
      | "s" -> player
      | "quit" -> failwith "unimplimented"
      | _ -> player_turn player state
    end
  end

(** [play_turns] returns a player list with each player's hand updated based
    on how many times they decided to hit *)
let rec play_turns state acc = 
  match state.players with 
    [] -> acc
  | x :: xs -> let new_player = player_turn x state in 
    let new_state = {players=xs; house=state.house} in 
    play_turns new_state (new_player :: acc)

(** [deal_cards] returns a player list where each player's hand is updated 
    to contain two cards randomly selected from the deck *)
let rec deal_cards state acc = 
  match state.players with 
    [] -> acc 
  | x :: xs -> let new_player = initialize_hand x in 
    let new_state = {players=xs; house=state.house} in 
    deal_cards new_state (new_player :: acc)

(** [place_bets] returns a players list where every player's bets for the 
    current round have been placed *)
let rec place_bets state acc = 
  match state.players with 
  | [] -> acc
  | x :: xs -> 
    print_endline "\n";
    print_string (x.name ^ "'s " ^ "hand and current balance:\n");
    print_cards x.hand;
    print_string ("Current balance: " ^ string_of_int x.balance ^ "\n");
    print_string "HOUSE's card:\n";
    print_cards [(List.hd (state.house.hand))];
    ANSITerminal.(print_string [red] "How much would you like to bet?\n" );
    print_string  "> ";
    match int_of_string_opt (read_line()) with
    | Some n when n <= x.balance ->
      let new_player = update_player_bet x n in 
      let new_state = {players=xs; house=state.house} in
      place_bets new_state (acc @ [new_player])
    | Some n -> ANSITerminal.(print_string [red] 
                                "\n\nYou cannot bet more than you have."); 
      place_bets state acc  
    | None -> print_string "\nInvalid input. Try again.\n"; place_bets state acc

(** [determine_balances] returns a players list with updated balances based 
    on how every player did against the house *)
let determine_balances state = 
  let house_score = get_sum state.house in 
  let rec determine_balances_helper players acc = 
    match players with 
    | [] -> acc 
    | x :: xs -> let new_player = update_balance x house_score in 
      determine_balances_helper xs (new_player :: acc) in 
  determine_balances_helper state.players []

(** [start_round] starts a new round of blackjack and returns the state once 
    the game is finished *)
let start_round state = 
  (* take cur_rotation and call the function that plays their turn *)
  let players_w_hands = deal_cards state [] in
  let house_with_hand = initialize_hand state.house in    
  let state_w_hands = {players=players_w_hands; 
                       house=house_with_hand} in  

  let players_w_bets = place_bets state_w_hands [] in 
  let state_w_bets = {players=players_w_bets; 
                      house=state_w_hands.house} in 

  let players_after_turns = play_turns state_w_bets [] in 
  let house_after_turn = player_turn state_w_bets.house state_w_bets in 

  let state_after_turns = {players = players_after_turns;
                           house=house_after_turn} in

  let players_w_balances = determine_balances state_after_turns in 
  let state_w_balances = {players=players_w_balances; 
                          house=state_after_turns.house} in 
  state_w_balances

(** [winners_list lst] returns a list of all of the winners of a round 
    represented by [lst] and their scores. *)
let winners_list (lst : (player * int) list) = 
  match lst with
  | (p, s) :: t -> List.filter (fun x -> snd(x) = s) lst
  | [] -> []
(* let max_vote_total = (snd (List.hd lst)) in
   List.filter (fun x -> snd(x) = max_vote_total) lst   *)

(** [output_multiple_winners winners_list] returns a string that contains the 
    names of all of the winners separated by a semicolon. *)
let rec output_multiple_winners winners_list score house_win= 
  match winners_list with
  | [] -> if house_win = false 
    then "are tied with a score of " ^ score ^ 
         (* (List.hd winners_list |> snd |> string_of_int) *)
         ", so they won this round. Congrats!\n" 
    else "are tied with a score of " ^ score ^ 
         ", so nobody won this round. Tough luck.\n"
  | h :: t -> if t = [] 
    then "and " ^ (fst h).name ^ " " ^ output_multiple_winners t score house_win
    else if List.length t = 1 
    then (fst h).name ^ " " ^ output_multiple_winners t score house_win
    else (fst h).name ^ ", " ^ output_multiple_winners t score house_win

(** [house_win player_sums] determines whether the house won *)
let house_win (player_sums : (player * int) list) : bool =
  let house_won = List.filter (fun x -> (fst(x)).name = "HOUSE") player_sums in
  if List.length house_won = 1 then true else false

(** [get_winner player_sums] determines the winner(s) of a list of players 
    and their scores [player_sums]. *)
let get_winner (player_sums : (player * int) list) : string =  
  let winners = winners_list player_sums in
  let house_win = house_win winners in
  if List.length winners = 1 && house_win
  then "\nThe House won this round with a score of " ^ 
       (List.hd winners |> snd |> string_of_int) 
       ^ ". Tough luck. \n"else
  if List.length winners = 1 then "\n" ^ (fst (List.hd winners)).name 
                                  ^ " won this round with a score of " 
                                  ^ (List.hd winners |> snd |> string_of_int) 
                                  ^ ". Congrats!\n"
  else if List.length winners = 0 
  then "\nThe House won this round. Tough luck.\n" 
  else if house_win
  then output_multiple_winners winners 
      (List.hd winners |> snd |> string_of_int) true else 
    output_multiple_winners winners 
      (List.hd winners |> snd |> string_of_int) false

(** [output_multiple_winners_game winners_list balance] prints the 
    winners within [winners_list] and their [balance]. *)
let rec output_multiple_winners_game winners_list balance = 
  match winners_list with
  | [] ->  "are tied with a balance of " ^ balance ^ 
           ", so nobody won this game. Tough luck.\n"
  | h :: t -> if t = [] 
    then "and " ^ (fst h).name ^ " " ^ output_multiple_winners_game t balance
    else if List.length t = 1 
    then (fst h).name ^ " " ^ output_multiple_winners_game t balance
    else (fst h).name ^ ", " ^ output_multiple_winners_game t balance

(** [get_winner player_money] determines the winner(s) of a list of players 
    and their balances [player_sums]. *)
let get_winner_game (player_money : (player * int) list) : string =  
  let winners = winners_list player_money in
  if List.length winners = 1 then "\n" ^ (fst (List.hd winners)).name 
                                  ^ " won this game with a balance of $" 
                                  ^ (List.hd winners |> snd |> string_of_int) 
                                  ^ ". Congrats!\n\n"
  else output_multiple_winners_game winners 
      (List.hd winners |> snd |> string_of_int)

(** [get_player_sums acc players] returns a dictionary [acc] where the key 
    is each player's name in [players] and the associated value is their 
    score at the end of the round. *)
let rec get_player_sums acc (players : player list) = 
  match players with 
  | [] -> acc
  | h :: t -> get_player_sums ((h, (get_sum h)) :: acc) t

(** [get_player_money acc players] returns a dictionary [acc] where the key 
    is each player's name in [players] and the associated value is their 
    balance at the end of the round. *)
let rec get_player_money acc (players : player list) = 
  match players with 
  | [] -> acc
  | h :: t -> get_player_money ((h, (get_balance h)) :: acc) t

(** [compare_players x y] compares [x] and [y] based on their scores. *)
let compare_players x y = 
  if snd x > snd y then 1 
  else if snd x < snd y then -1 
  else 0

(** [determine_round_winner determines the winner(s) of round [curr]. *)
let determine_round_winners curr = 
  let house_and_players = curr.house :: curr.players in 
  let player_sums = get_player_sums [] house_and_players in
  let not_elim = List.filter (fun x -> snd(x) <= 21) player_sums in  
  let sorted = List.sort compare_players not_elim |> List.rev in 
  let round_winners = get_winner sorted in 
  round_winners

(** [determine_round_winner determines the winner(s) of round [curr]. *)
let determine_game_winners curr = 
  let player_money = get_player_money [] curr.players in  
  let sorted = List.sort compare_players player_money |> List.rev in 
  let game_winners = get_winner_game sorted in 
  game_winners

(** [add_spaces name n] adds enough spaces to [name] to standardize the 
    string. *)
let rec add_spaces name n = 
  let len = String.length(name) in
  match len with 
  | x -> if x = n then name else add_spaces (name ^ " ") n

(** [standardize_name name] converts all [name] to the same length. *)
let standardize_name (name : string) = 
  let len = String.length(name) in
  if len >= 14 then String.sub name 0 14 
  else add_spaces name 14

(** [standardize_output output] converts all [output] to the same length. *)
let standardize_output (output : string) = 
  let len = String.length(output) in
  if len >= 13 then String.sub output 0 13 
  else add_spaces output 13

(** [determine_win player state] determines if [player] won in the round 
    represented by [state]. *)
let determine_win player state = 
  let house_score = sum_cards (get_hand state.house) in 
  let player_score = sum_cards (get_hand player) in 
  if player_score > 21 then false
  else if house_score > 21 || player_score > house_score then true else false

(** [determine_sign curr elt] determines the sign of the earnings of [elt] 
    at the end of round [curr]. *)
let determine_sign curr elt = 
  let player = fst elt in  
  if player.current_bet = 0 then "" else 
  if determine_win player curr then "" else "-"

(** [leader_entry acc lst] creates a leaderboard entry for each player in 
    [lst]. *)
let rec leader_entry acc lst curr = 
  match lst with 
  | [] -> acc
  | h :: t -> leader_entry 
                (acc ^ "* " ^ standardize_name((fst h).name) 
                 ^ "\t||\t"^ determine_sign curr h ^ "$" ^ 
                 ((fst h).current_bet |> string_of_int |> standardize_output) 
                 ^ "\t||\t$"
                 ^ (snd h |> string_of_int |> standardize_output) 
                 ^ " \t*\n") t curr

(** [print_round_leaderboard curr] prints the leaderboard at the end of 
    a given round [curr]. *)
let print_round_leaderboard curr = 
  let player_money = get_player_money [] curr.players in  
  let sorted = List.sort compare_players player_money |> List.rev in 
  let leaderboard_entries = leader_entry "" sorted curr in 
  "\n*************************************************************************\n"
  ^ 
  "*        PLAYER\t        ||    ROUND EARNINGS    ||    CURRENT BALANCE   *\n"
  ^ 
  "*-----------------------------------------------------------------------*\n"
  ^ leaderboard_entries ^ 
  "*************************************************************************\n"

(** [return_player player players] returns the corresponding [player] within
    [players]. *)
let rec return_player player players = 
  match players with
  | [] -> failwith "should never get here"
  | h :: t -> if h.name = player.name then h else return_player player t 

(** [determine_player_wins acc past_states player] determines the number of 
    rounds won by [player] in all prior rounds [past_states]. *)
let rec determine_player_wins acc past_states player = 
  match past_states with 
  | [] -> acc 
  | h :: t -> if determine_win (return_player player h.players) h
    then determine_player_wins (1 + acc) t player
    else determine_player_wins (acc) t player

(** [to_dollar] converts [float] into USD format. *)
let to_dollar float = 
  let float_string = string_of_float (float) in
  let period_spot = String.index float_string '.' in 
  let pre_decimal = String.sub float_string 0 (period_spot) in
  let post_decimal = if period_spot = (String.length float_string) - 1 
    then "00" else String.sub float_string (period_spot + 1) 2
  in pre_decimal ^ "." ^ post_decimal

(** [return_player_bet player players] returns the bet of [player] within 
    [players]. *)
let rec return_player_bet player players = 
  float_of_int (return_player player players).current_bet 

(** [determine_average_bet_amount acc past_states size player] determines
    the average bet amount of [player] throughout all prior rounds 
    [past_states]. *)
let rec determine_average_bet_amount acc past_states size player =   
  match past_states with 
  | [] -> acc /. float_of_int size
  | h :: t -> 
    determine_average_bet_amount 
      (return_player_bet player h.players  +. acc) t size player

(** [final_leader_entry acc lst] creates a leaderboard entry for each player in 
    [lst] at the end of the game. *)
let rec final_leader_entry acc lst curr past_states = 
  match lst with 
  | [] -> acc
  | h :: t -> final_leader_entry 
                (acc ^ "* " ^ standardize_name((fst h).name) 
                 ^ "\t||\t$" ^ (snd h |> string_of_int |> standardize_output) 
                 ^ "\t||\t" ^ (fst h |> determine_player_wins 0 past_states 
                               |> string_of_int |> standardize_output) 
                 ^ "\t||\t$" ^ 
                 (fst h |> determine_average_bet_amount 0.0 
                    past_states (List.length past_states) 
                  |>  to_dollar |> standardize_output) 
                 ^" \t*\n") t curr past_states

(** [print_game_leaderboard curr] prints the final leaderboard at the end of 
    a given round [curr]. *)
let print_game_leaderboard curr past_states = 
  let player_money = get_player_money [] curr.players in  
  let sorted = List.sort compare_players player_money |> List.rev in 
  let leaderboard_entries = final_leader_entry "" sorted curr past_states in 
  "\n*************************************************************************************************\n"
  ^ 
  "*        PLAYER\t        ||     FINAL BALANCE    ||     ROUNDS WON       ||  AVERAGE BET AMOUNT  *\n"
  ^ 
  "*-----------------------------------------------------------------------------------------------*\n"
  ^ leaderboard_entries ^ 
  "*************************************************************************************************\n"