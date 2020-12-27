type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

(* accepts a HW1 style grammar and converts it into a HW2 style grammar by recursively iterating through the 
rule list and returning rules that match the corresponding nonterminals*)
let convert_grammar gram1 = 
  let rec get_rules nt = function
   | [] -> []
   | (lhs, rhs)::t -> if lhs = nt then rhs::(get_rules nt t)
 				   else (get_rules nt t) in
  fst gram1, (fun nt -> get_rules nt (snd gram1))
 ;;

(*accepts a fragment and returns the derivation of the first acceptable prefix match based on the given grammars 
and acceptor*)
let parse_prefix gram accept frag = 
(*recursively match elements of a rule to fragment elements*)
let rec match_sym rules inrule accept deriv frag = match inrule with   | [] -> accept deriv frag
  | h::t ->  match h with
	| (N x) -> matcher x (rules x) rules (match_sym rules t accept) deriv frag
	| (T y) -> match frag with
		| [] -> None
		| lhs::rhs -> if lhs = y then match_sym rules t accept deriv rhs else None
(*recursively check for matches for each rule corresponding to a given nonterminal*)
and matcher nt inrules rules accept deriv frag = match inrules with 
  | [] -> None
  | h::t -> match (match_sym rules h accept (deriv@[(nt, h)])  frag) with
	| None -> matcher nt t rules accept deriv frag
	| Some z -> Some z in
(*begin matcher with initial nonterminal*)
matcher (fst gram) ((snd gram) (fst gram)) (snd gram) accept [] frag
;;

