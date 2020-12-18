open Hashtbl
open Player
open Card

module CardTup =
struct
  type t = int * int 
  let compare x y = match x, y with 
    | (x1, x2), (y1, y2) -> Stdlib.compare (x1+x2) (y1+y2)
end

(* Stay = 0, Hit = 1 *)

module CardTupMap = Map.Make(CardTup)
let decision_table = create 110

let stays = [(12, 4); (12, 5); (12, 6); (13, 2); (13, 3); (13, 4); (13, 5); 
             (13, 6); (14, 2); (14, 3); (14, 4); (14, 5); (14, 6); (15, 2); 
             (15, 3); (15, 4); (15, 5); (15, 6); (16, 2); (16, 3); (16, 4); 
             (16, 5); (16, 6)]
let hits = [(12, 2); (12, 3); (12, 7); (12, 8); (12, 9); (12, 10); (12, 1); 
            (13, 7); (13, 8); (13, 9); (13, 10); (13, 1); (14, 7); (14, 8); 
            (14, 9); (14, 10); (14, 1); (15, 7); (15, 8); (15, 9); (15, 10); 
            (15, 1); (16, 7); (16, 8); (16, 9); (16, 10); (16, 1)]

let rec insert_tup_list tup_list entry = 
  match tup_list with 
  | [] -> ()
  | x :: xs -> add decision_table x entry; insert_tup_list xs entry  

let initialize_decision_table () = 
  insert_tup_list stays 0; 
  insert_tup_list hits 1 

(* 11 or less and the AI hits every time *)
(* 17 or more and the AI should stay every time *)
let best_move player dealer_hand = 
  let player_sum = get_sum player in 
  if player_sum <= 11 then 1 else 
  if player_sum >= 17 then 0 else 
    let card_tup = (player_sum, dealer_hand) in 
    find decision_table card_tup
