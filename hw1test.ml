(* Homework 1. Fixpoints and grammar filters *)

let subset_test0 = subset [] [1;2;3]
let subset_test1 = subset [3;1;3] [1;2;3]
let subset_test2 = not (subset [1;3;7] [4;1;3])

let equal_sets_test0 = equal_sets [1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])

let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []
