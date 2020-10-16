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

let get_suit (c : card) = 
  c.suit

let get_value (c : card) = 
  c.value

let string_of_suit (c : card) = 
  let suit = get_suit c in 
  match suit with 
  | Clubs -> "clubs"
  | Spades -> "spades"
  | Hearts -> "hearts"
  | Diamonds -> "diamonds"

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
  string_of_int(int_of_value c) ^ " of " ^ string_of_suit c