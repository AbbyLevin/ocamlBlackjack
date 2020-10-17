type suit = 
  | Clubs
  | Spades
  | Hearts
  | Diamonds

type value = 
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = {suit: suit; value: value}

(** [create_card suit value] creates a card record with suit [suit] and 
     value [value]. *)
let create_card suit value = 
  {suit: suit; value: value}

(** [tuple_to_card tup] converts a suit * value tuple [tup] into a card. *)
let tuple_to_card (tup : suit * value) : card = 
  match tup with 
  | (x, y) -> 
    {suit=x; value=y}

(** [create_card_list acc lst] returns a list of cards corresponding to a list
    of suit * value tuples [lst]. *)
let rec create_card_list (acc : card list) 
    (lst : (suit * value) list) : card list = 
  match lst with 
  | [] -> acc
  | h :: t -> create_card_list (tuple_to_card h :: acc) t

(** [get_suit c] returns the suit of [c]. *)
let get_suit (c : card) : suit = 
  c.suit

(** [get_value c] returns the value of [c]. *)
let get_value (c : card) : value = 
  c.value

(** [string_of_suit suit] returns a string representation of [suit]. *)
let string_of_suit (suit : suit) = 
  match suit with 
  | Clubs -> "clubs"
  | Spades -> "spades"
  | Hearts -> "hearts"
  | Diamonds -> "diamonds"

(** [string_of_value value] returns a string representation of [value]. *)
let string_of_value (value : value) = 
  match value with 
  | One -> "1"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "jack"
  | Queen -> "queen"
  | King -> "king"
  | Ace -> "ace"

(** [int_of_value value] returns an int representation of [value]. 
    Note: Ace has weird behavior which we will handle in a later module. 
    For now, the default value of Ace is 1. *)
let int_of_value (value : value) = 
  match value with 
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 10
  | Queen -> 10
  | King -> 10
  | Ace -> 1

(** [string_of_card c] returns a string representation of [c]. *)
let string_of_card (c : card) = 
  (get_value c |> string_of_value) ^ " of " ^ (get_suit c |> string_of_suit)

(* [add_cards acc lst] returns the sum of the values of the cards in [lst]. *)
let rec add_cards (acc : int) (lst : card list) = 
  match lst with 
  | [] -> acc
  | h :: t -> add_cards (acc + (get_value h |> int_of_value)) t 

(** [add_card_to num c] adds the value of [c] to [num]. 
    Note: We would likely keep track of the current sum of a deck and 
    then use this as opposed to the method above. *)
let add_card_to (num : int) (c : card) : int = 
  num + (get_value c |> int_of_value)

let rec combine_suit_and_values curr suit value_list = 
  match value_list with 
  | [] -> curr
  | h :: t -> combine_suit_and_values ((suit, h) :: curr) suit t

let rec standard_deck_maker acc suit_list value_list = 
  match suit_list with 
  | [] -> acc
  | h :: t -> combine_suit_and_values acc h value_list

let create_standard_deck : card list = 
  let suit_list = [Clubs; Spades; Hearts; Diamonds] in 
  let value_list = [One; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; 
                    Jack; Queen; King; Ace] in
  standard_deck_maker [] suit_list value_list |> create_card_list []