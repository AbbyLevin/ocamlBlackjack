open Card
open Deck

(** The type representing a player. *)
type player = {name: string; hand: card list}

(** [create_player name hand] creates a player with [name] and [hand]. *)
let create_player name hand =
  {name = name; hand = hand}

(** [get_hand player] is the hand of [player]. *)
let get_hand player =
  player.hand

(** [get_sum player] is the sum of [player]. *)
let get_sum player =
  sum_cards player.hand

(** [initialize_hand player] deals two cards to [player]. *)
let initialize_hand player = 
  let card1 = get_card () in 
  let card2 = get_card () in 
  let init_hand = [card1; card2] in 
  {name=player.name; hand=init_hand}

(** [hit player state] carries out the functionality of hit by updating 
    [player] and [state]. *)
let hit player state =
  let new_card = get_card () in 
  let new_hand = new_card :: player.hand in 
  {name=player.name; hand=new_hand}

(** [house_turn player state] takes the houses turn according to house rules. *)
let house_turn player state = 
  if sum_cards player.hand < 17 
  then hit player state 
  else player 