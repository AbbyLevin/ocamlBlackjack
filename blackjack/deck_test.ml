open OUnit2
open Deck
open Card
open Player
open State

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [card_attribute_test name card_attrib suit value expected_output printer]
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] [create_card suit value |> card_attribute]. *)
let card_attribute_test
    (name : string) (card_attrib)
    (suit : suit) (value : value) 
    (expected_output) (printer): test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (create_card suit value |> card_attrib)
        ~printer:printer) 

(** [card_test name suit value expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with the string representation of [create_card suit value]. *)
let card_test 
    (name : string) 
    (suit : suit) (value : value)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (string_of_card_test(create_card suit value)) 
        ~printer:(fun x -> x))

(** [cards_sum_test name card_list expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [add_cards 0 card_list]. *)
let card_sum_test 
    (name : string) (card_list : card list)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (add_cards 0 card_list) 
        ~printer:(string_of_int)) 

(** [add_card_to_test name num suit value expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [add_card_to num (create_card [suit] [value]]. *)
let add_card_to_test 
    (name : string) (num: int) (suit : suit) (value : value)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (add_card_to num (create_card suit value)) 
        ~printer:(string_of_int))

(** [sum_cards name card_list expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [sum_cards card_list]. *)
let sum_cards_test 
    (name : string) (card_list : card list)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (sum_cards card_list) 
        ~printer:(string_of_int)) 

let card_tests =
  [
    card_attribute_test "testing that the suit is properly created" get_suit
      Clubs Two Clubs (string_of_suit);
    card_attribute_test "testing that the value is properly created" get_value
      Clubs Two Two (string_of_value);
    card_attribute_test "testing that the value is properly created face card" 
      get_value Clubs Ace Ace (string_of_value);
    card_test "testing card creation for non-face cards" Clubs Two 
      "2 of clubs";
    card_test "testing card creation for face cards" Diamonds Queen
      "queen of diamonds";
    card_test "testing card creation for ace cards" Spades Ace
      "ace of spades";
    card_sum_test "the sum of one card is the value of that card" 
      (create_card_list [] [(Spades, Two)]) 2;
    card_sum_test "the sum of two non-face cards" 
      (create_card_list [] [(Spades, Two); (Diamonds, Two)]) 4;
    card_sum_test "the sum of face cards" 
      (create_card_list [] [(Spades, Jack); (Diamonds, Queen)]) 20;
    card_sum_test "the sum of a mix of face and non-face cards" 
      (create_card_list [] 
         [(Spades, Jack); (Clubs, Four); (Hearts, Ten); (Diamonds, Queen)]) 34;
    add_card_to_test "adding an int to a non-face card" 5 Hearts Two 7;
    add_card_to_test "adding an int to a face card" 100 Spades King 110;

    (** Testing sum_cards *)
    sum_cards_test "empty hand" [] 0;
    sum_cards_test "one card" [{suit=Clubs; value=Two}] 2;
    sum_cards_test "10 cards" [{suit=Clubs; value=Two};
                               {suit=Clubs; value=Three};
                               {suit=Clubs; value=Four};
                               {suit=Clubs; value=Five};
                               {suit=Clubs; value=Six};
                               {suit=Clubs; value=Seven};
                               {suit=Clubs; value=Eight};
                               {suit=Clubs; value=Nine};
                               {suit=Clubs; value=Ten};
                               {suit=Clubs; value=Jack}] 64;
    sum_cards_test "ace high" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}] 21;
    sum_cards_test "ace low" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Two}] 13;
    sum_cards_test "one ace high one ace low" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Ace}] 12;
    sum_cards_test "one ace high two ace low" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Ace}; {suit=Clubs; value=Ace}] 13;
    sum_cards_test "one ace high two ace low, bust" 
      [{suit=Clubs; value=Ace}; {suit=Clubs; value=Jack}; 
       {suit=Clubs; value=Ten}; {suit=Clubs; value=Ace}; 
       {suit=Clubs; value=Ace}] 23;
  ]

(** [standard_deck_test name deck expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [deck]. *)
let standard_deck_test 
    (name : string) (deck)
    (expected_output : card list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output deck ~cmp:cmp_set_like_lists 
        ~printer:(pp_list string_of_card))

let deck_tests =
  [
    standard_deck_test "testing that a standard deck was properly created" 
      create_standard_deck 
      (create_card_list [] 
         [ (Clubs, Two); (Clubs, Three); (Clubs, Four); 
           (Clubs, Five); (Clubs, Six); (Clubs, Seven); (Clubs, Eight); 
           (Clubs, Nine); (Clubs, Ten); (Clubs, Jack); (Clubs, Queen); 
           (Clubs, King); (Clubs, Ace); 
           (Spades, Two); (Spades, Three); (Spades, Four); 
           (Spades, Five); (Spades, Six); (Spades, Seven); (Spades, Eight); 
           (Spades, Nine); (Spades, Ten); (Spades, Jack); (Spades, Queen); 
           (Spades, King); (Spades, Ace); 
           (Hearts, Two); (Hearts, Three); (Hearts, Four); 
           (Hearts, Five); (Hearts, Six); (Hearts, Seven); (Hearts, Eight); 
           (Hearts, Nine); (Hearts, Ten); (Hearts, Jack); (Hearts, Queen); 
           (Hearts, King); (Hearts, Ace); 
           (Diamonds, Two); (Diamonds, Three); 
           (Diamonds, Four); (Diamonds, Five); (Diamonds, Six); 
           (Diamonds, Seven); (Diamonds, Eight); (Diamonds, Nine); 
           (Diamonds, Ten); (Diamonds, Jack); (Diamonds, Queen); 
           (Diamonds, King); (Diamonds, Ace)]);
  ]

(** [state_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [List.length state]. *)
let state_test 
    (name : string) (state)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (List.length state) 
        ~printer:(string_of_int)) 

(** [state_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [List.length state]. *)
let house_test 
    (name : string) (state)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (state) 
        ~printer:(fun x -> x)) 

(** [print_player_int tuple] returns a string representation of [tuple]. *)
let print_player_int (tuple : Player.player * int) = 
  ((fst tuple).name ^ ", " ^ string_of_int(snd tuple))    

(** [player_sums_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [player_sums_list]. *)
let player_sums_test 
    (name : string) (player_sums_list : (Player.player * int) list)
    (expected_output) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (player_sums_list) 
        ~printer:(pp_list print_player_int))

(** [winner_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [determine_round_winners state]. *)
let winner_test 
    (name : string) (state)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (state)  
        ~printer:(fun x -> x)) 

let standard_state = init_game_state ["Austin"; "Abby"; "Brennan"] 100
let player_sums = get_player_sums [] standard_state.players

let string_of_hand hand = 
  pp_list string_of_card_test hand

let string_of_player player = 
  "Name: " ^ player.name ^ ", Current Balance = $"  
  ^ string_of_int (player.balance) ^ ", Hand: " ^ string_of_hand player.hand 
  ^ ", Current Bet: " ^ string_of_int player.current_bet ^ "\n"

let rec state_cycle acc = function
  | [] -> acc
  | h :: t -> state_cycle (acc ^ string_of_player h) t

let string_of_state curr = 
  string_of_player curr.house ^ state_cycle "" curr.players

(** [winner_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [determine_round_winners state]. *)
let create_state_test 
    (name : string) (state)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (string_of_state state)  
        ~printer:(fun x -> x))  

let p1 = {name= "Brennan"; hand = []; balance =  100; current_bet =  0} 
let p2 = {name= "Austin"; hand = []; balance =  100; current_bet =  0} 
let p3 = {name= "Abby"; hand = []; balance =  100; current_bet =  0} 
let default_house = {name="HOUSE"; hand=[]; 
                     balance=max_int; current_bet=0}
let default_state = {players = [p1; p2; p3]; house = default_house}

let p2_10 = {name="Austin"; hand=[{suit=Diamonds;value=Ten}]; 
             balance=max_int; current_bet=0}
let house_10 = {name="HOUSE"; hand=[{suit=Hearts;value=Ten}]; 
                balance=max_int; current_bet=0}
let p2_8 = {name="Austin"; hand=[{suit=Spades;value=Eight}]; 
            balance=max_int; current_bet=0}
let p1_8 = {name="Brennan"; hand=[{suit=Clubs;value=Eight}]; 
            balance=max_int; current_bet=0}

let house_win_state = {players = [p1; p2; p3]; house = house_10}
let house_tie_state = {players = [p1; p2_10; p3]; house = house_10}
let p2_8_win_state = {players = [p1; p2_8; p3]; house = default_house}
let p1_p2_tie_state = {players = [p1_8; p2_8; p3]; house = default_house}

let state_tests =
  [
    state_test "testing that init_game_state creates the proper size 
       player list" standard_state.players 3;
    house_test "testing that init_game_state creates the proper size 
       house list" standard_state.house.name "HOUSE";
    state_test "testing that init_game_state works as intended" 
      player_sums 3;
    (* player_sums_test "testing player sums" 
       (get_player_sums [] standard_state.players) []; *)
    (* player_sums_test "testing winners_list" 
       (winners_list player_sums) []; *)
    winner_test "testing get_winner of standard state" (get_winner player_sums) 
      "Brennan, Abby and Austin are tied with a score of 0, so they all won this round. Congrats!\n";  

    create_state_test "testing default state creation" (default_state) 
      "Name: HOUSE, Current Balance = $4611686018427387903, Hand: [], Current Bet: 0
Name: Brennan, Current Balance = $100, Hand: [], Current Bet: 0
Name: Austin, Current Balance = $100, Hand: [], Current Bet: 0
Name: Abby, Current Balance = $100, Hand: [], Current Bet: 0
";
    winner_test "testing determine_round_winner with all tied with house" 
      (determine_round_winners default_state) 
      "HOUSE, Brennan, Austin and Abby are tied with a score of 0, so nobody won this round. Tough luck.\n";
    winner_test "testing determine_round_winner with house solo dub" 
      (determine_round_winners house_win_state) 
      "\nThe House won this round with a score of 10. Tough luck. \n"; 
    winner_test "testing determine_round_winner with one tied with house" 
      (determine_round_winners house_tie_state) 
      "HOUSE and Austin are tied with a score of 10, so nobody won this round. Tough luck.\n"; 
    winner_test "testing determine_round_winner with non-house winner" 
      (determine_round_winners p2_8_win_state) 
      "\nAustin won this round with a score of 8. Congrats!\n";
    winner_test "testing determine_round_winner with non-house winners" 
      (determine_round_winners p1_p2_tie_state) 
      "Brennan and Austin are tied with a score of 8, so they all won this round. Congrats!\n";  
  ]


let tests =
  "test suite for Blackjack"  >::: List.flatten [
    card_tests;
    deck_tests;
    state_tests;
  ]

let _ = run_test_tt_main tests
