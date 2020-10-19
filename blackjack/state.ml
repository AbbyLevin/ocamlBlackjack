open Player 
open Card 
open Deck 

type state = {deck: card list; players: player list; cur_rotation: player list}

(** [init_game_state player_names] returns a game state with as many players
    as are in [player_names] plus the house *)
let init_game_state player_names = 
  let deck = shuffle create_standard_deck in 
  let players = (List.map (fun x -> {name=x; hand=[]}) player_names) in 
  let players_house = {name="HOUSE"; hand=[]} :: players in 
  {deck=deck; players=players; cur_rotation=players_house}
