fun same_string (s1 : string, s2 : string) =
    s1 = s2

(*problem 1*)
(* part (a) *)
fun all_except_option (a, xs) =
    case xs of
	[] => NONE
      | x::xs' => case same_string(x, a) of
		      true => SOME (xs')
		    | false => case all_except_option(a, xs') of
				   SOME a => SOME (x :: a)
				  |NONE => NONE 
	     
(* part (b) *)
fun get_substitutions1 (substitutions, s) =
    case substitutions of
	[] => []
      | x :: xs'  => case all_except_option (s, x) of
		       SOME a => a @ get_substitutions1(xs', s)
		     | NONE => get_substitutions1(xs', s)

(* part (c) *)
fun get_substitutions2 (substitutions, s) =
    let
	fun helper (substitutions, acc) =
	    case substitutions of
		[] => acc
	      | x :: xs'  => case all_except_option (s, x) of
			       SOME a => helper(xs', a @ acc)
			     | NONE => helper(xs', acc)
    in
	helper(substitutions, [])
    end

(* part (d) *)
fun similar_names (substitutions, {first = name, middle = second_name, last = surname}) =
    let 
	fun helper (fullList) =
		   case fullList of
		       [] => []
		     | x :: xs' => {first = x, middle = second_name, last = surname} :: helper(xs')
    in
	helper (name :: get_substitutions2(substitutions, name))
    end
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* problem 2 *)
(* part (a) *)
fun card_color (cardSuit, cardRank) =
    case cardSuit of
	Clubs => Black
      | Spades => Black
      | _  => Red
	      
(* part (b) *)
fun card_value (cardSuit, cardRank) =
    case cardRank of
	 Num a => a
      | Ace => 11
      | _ => 10
  
(* part (c) *)
fun remove_card (cs, c, e) =
    let
	fun helper cs =
	    case cs of
		[] => raise e
	      | s :: cs' => if s = c
			    then cs'
			    else s :: helper cs'
    in
	helper cs
    end

(* part (d) *)
fun all_same_color cs =
    case cs of
	[] => true
      | _ :: [] => true
      | x :: x' :: [] => card_color x = card_color x'
      | x :: x' :: xs' =>  card_color x = card_color x' andalso all_same_color (x :: xs')
									       
(* part (e) *)
fun sum_cards cs =
    let
	fun helper (cs, acc) =
	    case cs of
		[] => acc
	      | s :: cs' => helper(cs', card_value s + acc)
    in
	helper (cs, 0)
    end
 
(* part (f) *)
fun score (heldCards, goal) =
    let
	val sum = sum_cards heldCards
    in
	case (sum > goal, all_same_color heldCards) of
	    (true, false) => 3 * (sum - goal)
	  | (false, false) => goal - sum
	  | (true, true) => 3 * (sum - goal) div 2
	  | (false, true) => (goal - sum) div 2 
    end

(* part (g) *)
fun officiate (cs, moveList, goal) =
    let
	fun helper (heldCards, cs, moveList) =
	    case (heldCards, cs, moveList) of
		(_, _, []) => score (heldCards, goal)
	      | (_, _, (Discard c) :: moves) => helper (remove_card (heldCards, c, IllegalMove), cs, moves)
	      | (_, [], Draw :: moves) => score (heldCards, goal)
	      | (_, c :: cs', Draw :: moves) => if sum_cards heldCards > goal
						then score (heldCards, goal)
						else helper (c :: heldCards, cs', moves)
						
    in
	helper ([], cs, moveList) 
    end
(*	      
(* challenge problems *)
(* part (a) *)
fun score_challenge () =

fun officiate_challenge () =
    
(* part (b) *)	      
fun careful_player () =
*)
