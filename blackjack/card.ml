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

let create_card suit value = 
  {suit: suit; value: value}

let tuple_to_card tup = 
  match tup with 
  | (x, y) -> 
    {suit=x; value=y}

let rec create_card_list acc (lst : (suit * value) list) : card list = 
  match lst with 
  | [] -> acc
  | h :: t -> create_card_list (tuple_to_card h :: acc) t

let get_suit (c : card) = 
  c.suit

let get_value (c : card) = 
  c.value

let string_of_suit (suit : suit) = 
  (*let suit = get_suit c in *)
  match suit with 
  | Clubs -> "clubs"
  | Spades -> "spades"
  | Hearts -> "hearts"
  | Diamonds -> "diamonds"

let string_of_value (value : value) = 
  (*let value = get_value c in *)
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

(* Ace has a lil bit of weird behavior *)
let int_of_value (c : card) = 
  let value = get_value c in 
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

let string_of_card (c : card) = 
  (get_value c |> string_of_value) ^ " of " ^ (get_suit c |> string_of_suit)

(*can likely do this functionality in Deck instead, but we'll see if it gets
  used *)
let rec add_cards acc (lst : card list) = 
  match lst with 
  | [] -> acc
  | h :: t -> add_cards (acc + int_of_value h) t 
