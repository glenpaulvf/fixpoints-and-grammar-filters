(* Homework 1. Fixpoints and grammar filters *)

let rec member x s =
	match s with
	| [] -> false
	| h::t when h = x -> true
	| h::t -> member x t

 let rec subset a b =
	match a with
	| [] -> true
	| h::t -> if member h b then subset t b else false

let equal_sets a b =
	subset a b && subset b a

let add_element x s =
	if member x s then s else x::s

let rec set_union a b =
	match a with
	| [] -> b
	| h::t -> add_element h (set_union t b)

let rec set_intersection a b =
	match a with
	| [] -> []
	| h::t -> if member h b then add_element h (set_intersection t b) else set_intersection t b

let rec set_diff a b =
	match a with
	| [] -> []
	| h::t -> if not (member h b) then add_element h (set_diff t b) else set_diff t b

let fixed_point eq f x =
	if eq (f x) x then true else false

let rec computed_fixed_point eq f x =
	if fixed_point eq f x then x else computed_fixed_point eq f (f x)

let rec computed_periodic_point eq f p x =
	match p with
	| 0 -> if fixed_point eq f x then p else -1
	| _ -> if fixed_point eq f x then p else computed_periodic_point eq f (p - 1) (f x)

