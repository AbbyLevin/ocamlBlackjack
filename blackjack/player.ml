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
