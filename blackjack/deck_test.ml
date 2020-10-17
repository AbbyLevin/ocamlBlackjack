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
      assert_equal expected_output 
        (add_cards 0 card_list) 
        ~printer:(string_of_int)) 

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
  ]

let tests =
  "test suite for Blackjack"  >::: List.flatten [
    card_tests;

  ]

let _ = run_test_tt_main tests
