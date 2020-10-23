open Card
open Deck

(** The type representing a player. *)
type player = {name: string; hand: card list}

let create_player name hand =
  {name = name; hand = hand}

(** [get_hand player] is the hand of [player] *)
let get_hand player =
  player.hand

(** [get_sum player] is the sum of [player] *)
let get_sum player =
  sum_cards player.hand

let initialize_hand player = 
  let card1 = get_card () in 
  let card2 = get_card () in 
  let init_hand = [card1; card2] in 
  {name=player.name; hand=init_hand}

(** *)
let hit player state =
  let new_card = get_card () in 
  let new_hand = new_card :: player.hand in 
  {name=player.name; hand=new_hand}