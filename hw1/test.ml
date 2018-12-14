(* hw1.ml *)

(* helper function *)
let rec is_element a lst =
	match lst with
	[] ->  false
	| h::t -> if a = h then true else is_element a t;;

(* 1. Write a function subset a b that returns true iff a is a subset of b *)	
let rec subset a b = 
	match a with
	[] -> true
	| h::t -> if (is_element h b) = false then false
	          else subset t b;;