(* Homework 1. Fixpoints and grammar filters *)

let rec (member : 'a -> 'a list -> bool) =
	fun x s ->
	match s with
	| [] -> false
	| h::t when h = x -> true
	| h::t -> member x t

 let rec (subset : 'a list -> 'a list -> bool) =
	fun a b ->
	match a with
	|	[] -> true
	| h::t -> if (member h b) then subset t b else false

let rec (equal_sets : 'a list -> 'a list -> bool) =
	fun a b ->
	subset a b && subset b a

let rec (add_element : 'a -> 'a list -> 'a list) =
	fun x s ->
	if member x s then s else x::s

let rec (set_union : 'a list -> 'a list -> 'a list) =
	fun a b ->
	match a with
	| [] -> b
	| h::t -> add_element h (set_union t b)
