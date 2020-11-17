open Card
open Deck
open Player
open State

(** [play_round state here] carries out all of the functionality of playing a 
    round of Blackjack with associated game information [state]. [here] 
    is used to prevent this functionality from occuring when the 
    file is loaded. *)
val play_round : state -> bool -> state 

(** [repeat_rounds state quit here] allows a user to play a round and 
    update [state] along the way as long as they don't make [quit] true. 
    [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
val repeat_rounds : state -> bool -> bool -> unit

(** [prompt_name here] prompts a user to enter their name and handles
    their response. [here] is used to prevent this functionality from 
    occuring when the file is loaded. *)
val prompt_name : bool -> string

(** [get_names name_list n] returns [name_list] which has the names of the
    [n] players. *)
val get_names : string list -> int -> string list

(** [initialize_game here] initializes the game by gathering the number of
    players and returning an initial game state with each of these players' 
    names. [here] is used to prevent this functionality from occuring when the 
    file is loaded.*)
val initialize_game : bool -> int -> state 

(** [rules here] handles outputting the rules if a player desires them. *)
val rules : bool -> unit 

(** [play_game here] starts the Blackjack game. [here] is used to prevent
    this functionality from occuring when the file is loaded. *)
val play_game : bool -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit 