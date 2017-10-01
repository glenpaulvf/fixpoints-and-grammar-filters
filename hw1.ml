(* Homework 1. Fixpoints and grammar filtrs *)

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
	| h::t ->
		if (member h b) then subset t b 
		else false

