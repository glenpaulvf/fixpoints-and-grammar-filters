(* Homework 1. Fixpoints and grammar filters *)

let my_subset_test0 = subset [1;2;3;4] [1;2;3;4;4]

let my_equal_sets_test0 = equal_sets [1;2;3] [1;2;2;2;2;3;3;3]

let my_set_union_test0 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]

let my_set_intersection_test0 = equal_sets (set_intersection [1;2] [2;3;2;2]) [2]

let my_set_diff_test0 = equal_sets (set_diff [1;2] [2;3;4;5]) [1]

let my_computed_fixed_point_test0 = (computed_fixed_point (=) (fun x -> x / 4) 10000) = 0

let my_computed_periodic_point_test0 = (computed_periodic_point (=) (fun x -> x / 4) 0 (1)) = 1

let my_while_away_test0 = equal_sets (while_away ((+) 2) ((>) 15) 0) [0; 2; 4; 6; 8; 10; 12; 14]

let my_rle_decode_test0 = equal_sets (rle_decode [12,"W"; 1, "B"; 12, "W";]) ["W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "B"; "W"; "W";
 "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"; "W"]

type sample_nonterminals =
  | S | A | B

let sample_grammar =
  S,
  [S, [N A];
  S, [N B];
  A, [N A];
  A, [T "a"; N B];
  A, [T "a"; N A];
  A, [T "a"];
  B, [N B]]

let my_filter_blind_alleys_test0 = (filter_blind_alleys sample_grammar) = (S, [(S, [N A]); (A, [N A]); (A, [T "a"; N A]); (A, [T "a"])])

