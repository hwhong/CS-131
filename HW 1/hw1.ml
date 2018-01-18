let rec find e l = match l with 
	| [] -> false
	| h::t -> if h = e then true else find e t

(* 1. if the set represented by the list a is a subset of the set represented by the list b *)
let rec subset a b = match a with 
	| [] -> true
	| h::t -> (find h b) && (subset t b)

(* Helper function to find the length of list *)
let rec length l = match l with 
	| [] -> 0
	| h::t -> 1 + length t

(* 2. returns true iff the represented sets are equal *)
let equal_sets a b = (subset a b) && (subset b a) 

(* 3. returns a list representing a union b *)
let rec set_union a b = match b with 
	| [] -> a 
	| h::t -> if find h a then set_union a t else h :: set_union a t 

(* 4. returns a list representing a intersection b *) 
let rec set_intersection a b = match a with
    | [] -> []
    | h::t -> if find h b then h :: (set_intersection t b) else set_intersection t b

(* 5. returns a list representing a − b, that is, the set of all members of a that are not also members of b *)
let rec set_diff a b = match a with
	| [] -> []
	| h::t -> if not (find h b) then h :: (set_diff t b) else set_diff t b

(* 6. returns the computed fixed point for f with respect to x, assuming that eq is the equality predicate for f's domain *)
let rec computed_fixed_point eq f x = 
	if eq x (f x) then (f x) 
	else computed_fixed_point eq f (f x)  

(* 7. returns the computed periodic point for f with period p and with respect to x, 
 *    assuming that eq is the equality predicate for f's domain. *)
let rec computed_periodic_point eq f p x = 
	if p = 0 then x 
	else 
		if eq x (f x) then let temp = p - 1 in computed_periodic_point eq f temp (f x) 
		else computed_periodic_point eq f p (f x)
	

(* 8. returns the longest list [x; s x; s (s x); ...] such that p e is true for every element e in the list. *)
let rec while_away s p x = match (p x) with 
	| false -> []
	| true -> x :: while_away s p (s x)

(* Helper function to return c (count) e's (element) in a form of a list*)
let rec append c e = if c = 0 then [] else e :: append (c-1) e

(* 9. decodes a list of pairs lp in run-length encoding form *)
let rec rle_decode lp = match lp with
    | [] -> []
    | h::t -> append (fst h) (snd h) @ rle_decode t

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* ----------------------------- Remove Blind Alley Rule Starts Here ----------------------------- *)

(* Determines whether something is a terminal or not *)
let is_terminal rule = match rule with
  	| T x -> true
  	| N x -> false

(* returns a expression *)
let rec substitute expr rules = match rules with 
	| [] -> [expr] 
	| head::tail -> if (N (fst head) = expr) && ((snd head) = []) then [] else substitute expr tail

(* Goes through the RHS of rules and create wanted pairs as we go *)
let rec traverseRHS rule rules = match (snd rule) with 
	| [] -> ((fst rule), [])
	| head::tail -> ((fst rule), (substitute head rules)@(snd (traverseRHS ((fst rule), tail) rules)))

(* A recursive function that finds the next state of list 
for each element on the rhs, filter the non-terminals out *)
let rec parse rules fullList = match rules with 
	| [] -> [] 
	| head::tail -> (traverseRHS head fullList) :: (parse tail fullList)

(* Deletes the terminal *)
let rec delete_terminal rule = match rule with
	| [] -> []
	| head::tail -> if is_terminal head then delete_terminal tail else head :: delete_terminal tail 

(* if there is a terminal, removes it from the list *)
let rec filter rules = match rules with 
	| [] -> [] 
	| head::tail -> ((fst head), delete_terminal (snd head)) :: filter tail 

(* Wrapper to make the code look cleaner *)
let parse_wrapper rules = filter rules; parse rules rules

let compare_Expression rule1 rule2 = (rule1 = rule2)

(* Helper to see if both have the same set of rules *)
(* Example lists *)
(* [Expr, [T "("; N Expr; T")"]; 
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr]] *)
let rec equal_lists l1 l2 = match l1, l2 with
	| [], []               -> true
	| [], _ | _, []        -> false
	| x::xs, y::ys -> if not (compare_Expression x y) then false else equal_lists xs ys

let isEmpty l = match (snd l) with 
	| [] -> true
	| _ -> false

let rec final_filter l1 l2  = match l1, l2 with 
	| [], [] -> []
	| x::xs, y::ys -> if isEmpty y then x::(final_filter xs ys) else final_filter xs ys

(* 10. Recursively, using the computed_fixed_point function, see if we could reach the state of sameness.
   At that point, we know this is the grammar without the Blind alley rules*)
let filter_blind_alleys g = ((fst g), (computed_fixed_point equal_lists parse_wrapper (snd g)))


