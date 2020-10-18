open Card
open Deck

(** The type representing a player. *)
type player = {name: string; hand: card list; sum: int}

let create_player name hand sum =
  {name = name; hand = hand; sum = sum}

(** [get_hand player] is the hand of [player] *)
let get_hand player =
  player.hand

(** [get_sum player] is the sum of [player] *)
let get_sum player =
  player.sum

(** *)
let hit player deck =
  failwith "unimplimented"
(**   let top_card = List.fst deck in
      player.hand = top_card :: (get_hand player);
      List.remove deck top_card *)