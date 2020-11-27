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
    file whose name is [name]. 

    Precondition: [name] is the name of a valid blackjack json file in the same
    directory, ommitting the ".json" extension.   *)
let load_game name = 
  let json = Yojson.Basic.from_file (name ^ ".json") in 
  let players = json |> member "players" |> 
                to_list |> List.map player_of_savedp in
  {players=players; house=
                      {name="HOUSE"; hand=[]; balance=max_int; current_bet=0};
   game_name=name}

let remove_l_comma str = 
  let len_str = String.length str in 
  let res = String.sub str 0 (len_str-1) in 
  print_string res;
  res

let json_of_players state = 
  let players = state.players in 
  let res = List.fold_right (fun a b -> {|{"name": "|} ^ a.name ^ {|","balance": |} ^ (string_of_int a.balance) ^ "}," ^ b) players "" in 
  let res_formatted = remove_l_comma res in 
  let json_string = {|{"players": [|} ^ res_formatted ^ {|]}|} in 
  Yojson.Safe.from_string json_string

let save_game state =
  let game_name = state.game_name in 
  let json_to_write = json_of_players state in 
  Yojson.Safe.to_file (game_name ^ ".json")json_to_write