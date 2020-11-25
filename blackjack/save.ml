open Yojson.Basic.Util
open Player 
open State

(** [player_of_savedp] returns a player object based on the Yojson.Basic.t
    object in [saved_p]. *)
let player_of_savedp saved_p = 
  let name = saved_p |> member "name" |> to_string in 
  let balance = saved_p |> member "balance" |> to_int in 
  {name=name; hand=[]; balance=balance; current_bet=0}

(** [load_game name] returns a game state based on the contents from the json
    file whose name is [name]. *)
let load_game name = 
  let json = Yojson.Basic.from_file (name ^ ".json") in 
  let players = json |> member "players" |> 
                to_list |> List.map player_of_savedp in
  {players=players; house=
                      {name="HOUSE"; hand=[]; balance=max_int; current_bet=0};
   game_name=name}