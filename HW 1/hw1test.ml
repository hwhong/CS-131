let my_subset_test0 = subset [] [1;2;3]
let my_subset_test1 = not (subset [1;3;7] [4;1;3])
let my_subset_test2 = subset [1] [1]

let my_equal_sets_test0 = equal_sets [1;3] [1;3]
let my_equal_sets_test1 = not(equal_sets [1;3;4] [3;1;3])

let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1; 2] [3; 4]) [1; 2; 3; 4]

let my_set_intersection_test0 = equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test1 = equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]
let my_set_intersection_test2 = equal_sets (set_intersection [1] [3;1]) [1]

let my_set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4]
let my_set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let my_set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []
let my_set_diff_test4 = equal_sets (set_diff [1; 2; 3] [3]) [1; 2]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let my_computed_fixed_point_test2 = computed_fixed_point (=) sqrt 10. = 1.
let my_computed_fixed_point_test3 = ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			                              (fun x -> x /. 2.) 10.) = 1.25)
let my_computed_fixed_point_test4 = computed_fixed_point (=) (fun x -> x * 2) 2 = 4500

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
(* let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1. *)
let my_computed_periodic_point_test2 = computed_periodic_point (=) (fun x -> x / 4) 0 (-1) = -1

let my_while_away_test0 = while_away ((+) 3) ((>) 10) 0 = [0;3;6;9]

let my_rle_decode_test0 = rle_decode [2,0; 1,6] = [0; 0; 6]
let my_rle_decode_test1 = rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"] = ["w"; "w"; "w"; "x"; "z"; "z"]
let my_rle_decode_test2 = rle_decode [10, "A"; 0, "B"] = ["A";"A";"A";"A";"A";"A";"A";"A";"A";"A"]
let my_rle_decode_test3 = rle_decode [1, "H"; 1, "O"; 1, "N"; 1, "G"] = ["H";"O";"N";"G"]

(* An example grammar for a small subset of Awk, derived from but not
   identical to the grammar in
   <http://web.cs.ucla.edu/classes/winter06/cs132/hw/hw1.html>.  *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let awksub_test2 =
  filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])

let awksub_test3 =
  filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  filter_blind_alleys giant_grammar = giant_grammar

let giant_test1 =
  filter_blind_alleys (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let giant_test2 =
  filter_blind_alleys (Sentence, List.tl (List.tl (snd giant_grammar))) =
    (Sentence,
     [Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Grunt]; Sentence, [N Shout]])