open OUnit2
open Deck
open Card

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
      assert_equal expected_output (string_of_card(create_card suit value)) 
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

let shuffle_test 
    (name : string) (deck)
    (expected_output : card list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      (assert (expected_output != deck)))

let card_tests =
  [
    card_attribute_test "testing that the suit is properly created" get_suit
      Clubs One Clubs (string_of_suit);
    card_attribute_test "testing that the value is properly created" get_value
      Clubs One One (string_of_value);
    card_attribute_test "testing that the value is properly created face card" 
      get_value Clubs Ace Ace (string_of_value);
    card_test "testing card creation for non-face cards" Clubs One 
      "1 of clubs";
    card_test "testing card creation for face cards" Diamonds Queen
      "queen of diamonds";
    card_test "testing card creation for ace cards" Spades Ace
      "ace of spades";
    card_sum_test "the sum of one card is the value of that card" 
      (create_card_list [] [(Spades, Two)]) 2;
    card_sum_test "the sum of two non-face cards" 
      (create_card_list [] [(Spades, Two); (Diamonds, One)]) 3;
    card_sum_test "the sum of face cards" 
      (create_card_list [] [(Spades, Jack); (Diamonds, Queen)]) 20;
    card_sum_test "the sum of a mix of face and non-face cards" 
      (create_card_list [] 
         [(Spades, Jack); (Clubs, Four); (Hearts, Ten); (Diamonds, Queen)]) 34;
    add_card_to_test "adding an int to a non-face card" 5 Hearts One 6;
    add_card_to_test "adding an int to a face card" 100 Spades King 110;
  ]

let deck_tests =
  [
    standard_deck_test "testing that a standard deck was properly created" 
      create_standard_deck 
      (create_card_list [] 
         [(Clubs, One); (Clubs, Two); (Clubs, Three); (Clubs, Four); 
          (Clubs, Five); (Clubs, Six); (Clubs, Seven); (Clubs, Eight); 
          (Clubs, Nine); (Clubs, Ten); (Clubs, Jack); (Clubs, Queen); 
          (Clubs, King); (Clubs, Ace); 
          (Spades, One); (Spades, Two); (Spades, Three); (Spades, Four); 
          (Spades, Five); (Spades, Six); (Spades, Seven); (Spades, Eight); 
          (Spades, Nine); (Spades, Ten); (Spades, Jack); (Spades, Queen); 
          (Spades, King); (Spades, Ace); 
          (Hearts, One); (Hearts, Two); (Hearts, Three); (Hearts, Four); 
          (Hearts, Five); (Hearts, Six); (Hearts, Seven); (Hearts, Eight); 
          (Hearts, Nine); (Hearts, Ten); (Hearts, Jack); (Hearts, Queen); 
          (Hearts, King); (Hearts, Ace); 
          (Diamonds, One); (Diamonds, Two); (Diamonds, Three); 
          (Diamonds, Four); (Diamonds, Five); (Diamonds, Six); 
          (Diamonds, Seven); (Diamonds, Eight); (Diamonds, Nine); 
          (Diamonds, Ten); (Diamonds, Jack); (Diamonds, Queen); 
          (Diamonds, King); (Diamonds, Ace)]);
    standard_deck_test "testing that shuffle deck returns the same cards "
      (shuffle create_standard_deck) create_standard_deck;
    shuffle_test "testing that shuffle deck returns the cards in a different 
    order" (shuffle create_standard_deck) create_standard_deck;
  ]



let tests =
  "test suite for Blackjack"  >::: List.flatten [
    card_tests;
    deck_tests;
  ]

let _ = run_test_tt_main tests
