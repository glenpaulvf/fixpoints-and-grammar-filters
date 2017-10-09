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
	if member x s then s else List.cons x s

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

let rec periodic_point eq f p x =
	match p with
	| 0 -> x
	| _ -> periodic_point eq f (p - 1) (f x)

let rec computed_periodic_point eq f p x =
	if eq (periodic_point eq f p x) x then x else computed_periodic_point eq f p (f x)

let rec while_away s p x =
	if p x then List.cons x (while_away s p (s x)) else []

let rec decoding (n, v) =
	match n with
	| 0 -> []
	| _ -> List.cons v (decoding (n - 1, v))

let rec rle_decode lp =
	match lp with
	| [] -> []
	| (n, v)::t -> List.append (decoding (n, v)) (rle_decode t)

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

let rec evaluate_production prod generatives =
	match prod with
	| N nonterminal -> subset [nonterminal] generatives
	| T terminal -> true

let rec evaluate_rhs rhs generatives =
	match rhs with
	| [] -> true
	| h::t -> if evaluate_production h generatives then evaluate_rhs t generatives else false

 let rec evaluate_rules (rules, generatives) =
	match rules with
	| [] -> (rules, generatives)
	| (nt, rhs)::t -> if evaluate_rhs rhs generatives && not (subset [nt] generatives) then evaluate_rules (t, (nt::generatives)) else evaluate_rules (t, generatives)

let evaluate_rule_generatives_pair (rules, generatives) =
	let generative_list = snd (evaluate_rules (rules, generatives)) in
	(rules, generative_list)

let evaluate_fixed_point_rules rules =
	computed_fixed_point (fun (_, x) (_, y) -> equal_sets x y) evaluate_rule_generatives_pair (rules, [])

let rec filter_rules rules generatives =
	match rules with
	| [] -> []
	| (nt, rhs)::t -> if evaluate_rhs rhs generatives then (nt, rhs)::filter_rules t generatives else filter_rules t generatives

let filter_blind_alleys g =
	let start_symbol = fst g in
	let rules = filter_rules (snd g) (snd (evaluate_fixed_point_rules (snd g))) in
	(start_symbol, rules)

