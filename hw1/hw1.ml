(* hw1.ml *)

(* In this homework, we are allowed to use Pervasives module and List modiule. *)
(* Since the Pervasives module is imported by default, I only need to import List module *)

open List

(* 1. Write a function subset a b that returns true iff a⊆b *)

let rec subset a b = 
 	match a with
 		[] -> true
 		| hd :: tl -> if (List.mem hd b) then subset tl b
 					  else false;;

(* 2. Write a function equal_sets a b that returns true iff the represented sets are equal. *)

 let equal_sets a b =
 	if (subset a b) && (subset b a) then true else false;;

(* 3. Write a function set_union a b that returns a list representing a U b. *)

let set_union a b = 
	List.sort_uniq compare (List.append a b);;  (* I use sort_uniq function in List module to remove duplicate elements in the list. increasing order. *)

(* 4. Write a function set_intersection a b that returns a list representing a ∩ b. *)

let rec set_intersection a b =
	match a with
		[] -> []
		| hd :: tl -> if (List.mem hd b) then hd :: set_intersection tl b
					  else set_intersection tl b;;

(* 5. Write a function set_diff a b that returns a list representing a−b. *)

let rec set_diff a b =
	match a with
		[] -> []
		| hd :: tl -> if (List.mem hd b) then set_diff tl b
					  else hd :: set_diff tl b;;

(* 6. Write a function computed_fixed_point eq f x that returns the computed fixed point for f with respect to x, assuming that eq is the equality predicate for f's domain. *)

let rec computed_fixed_point eq f x = 
	if (eq (f x) x) then x
	else computed_fixed_point eq f (f x);;

(* 7. Write a function filter_reachable g that returns a copy of the grammar g with all unreachable rules removed. *)
(* This function should preserve the order of rules *)

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
	
let (=) a a = true;;
(*
let is_nonterminal_symb symb =
	match symb with
	N symbol-> true
	|T _-> false;;

let rec get_nonterminal_symb_from_list symbol_list =
	match symbol_list with 
		[] -> []
		|(symbol s)::tl -> if (is_nonterminal_symb hd) then hd :: (get_nonterminal_symb_from_list tl)
					else (get_nonterminal_symb_from_list tl);;

let rec remove_type_from_list nonT_list = 
	match nonT_list with
		[] -> []
		| N symbol :: tl -> symbol :: remove_type_from_list tl;;


let rec get_nonterminal_symb_from_list symbol_list =
	match symbol_list with 
		[] -> []
		|N symbol :: tl ->  symbol :: (get_nonterminal_symb_from_list tl)
		|T _ :: tl -> get_nonterminal_symb_from_list tl;;



let rec get_all_reachable_nonterminal start_nont_symb rules =  
	match rules with 
		[] -> []
		| hd::tl -> if (fst hd = start_nont_symb) then List.append get_nonterminal_symb_from_list (snd hd) get_all_reachable_nonterminal start_nont_symb tl
						else [];;


reachable_rules function returned a filtered rules from the original rules based on a list which contains all reachable nonterminal symbols 
let rec reachable_rules reachable_nont_symb_list rules = 
	match rules with
		[] -> []
		| (a, b)::tl -> if (List.mem a reachable_nont_symb_list) then (a, b) :: (reachable_rules reachable_nont_symb_list tl)
					else reachable_rules reachable_nont_symb_list tl;;
					
let filter_reachable g = 
	match g with
		[] -> []
		| (expr, rules) -> (expr, reachable_rules (get_all_reachable_nonterminal expr rules));;

let is_nonterminal_symb symb =
	match symb with
	N symbol-> true
	|T _-> false;;

Final try
let f2 start_nont_symb rules symbol_list =
	match symbol_list with
		nil -> []
		| T _ -> []
		| N symbol -> if (symbol = start_nont_symb) then []
					  else symbol :: build_nonT_list symbol rules;;
	
let rec f1 start_nont_symb rules rule = 
	match rule with
		[] -> []
		|(a,[b]) -> if (start_nont_symb = a) then List.map (f2 start_nont_symb rules) b
										     else [];;

let rec build_nonT_list start_nont_symb rules = 
	if (rules = []) then []
					else List.map (f1 rstart_nont_symb rules) rules;; --end with final try*)
let filter_reachable g = g
;;
