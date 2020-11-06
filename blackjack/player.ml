open Card
open Deck

(** The type representing a player. *)
type player = {name: string; hand: card list; balance: int; current_bet: int}

let create_player name hand =
  {name = name; hand = hand; balance = 0; current_bet = 0}

(** [get_hand player] is the hand of [player] *)
let get_hand player =
  player.hand

(** [get_sum player] is the sum of [player] *)
let get_sum player =
  sum_cards player.hand

(** [update_player_bet player] returns a new player record with the same 
    fields as [player] except for the sum field which is [new_sum] *)
let update_player_bet player new_sum = 
  {name = player.name; hand = player.hand; balance = player.balance-new_sum; 
   current_bet = new_sum}

let initialize_hand player = 
  let card1 = get_card () in 
  let card2 = get_card () in 
  let init_hand = [card1; card2] in 
  {name=player.name; hand=init_hand; 
   balance=player.balance; current_bet=player.current_bet}

(** *)
let hit player state =
  let new_card = get_card () in 
  let new_hand = new_card :: player.hand in 
  {name=player.name; hand=new_hand; 
   balance=player.balance; current_bet=player.current_bet}

let house_turn player state = 
  if sum_cards player.hand < 17 
  then hit player state 
  else player 