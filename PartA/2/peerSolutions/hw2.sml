 (* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a *)
fun all_except_option (s, []) = NONE
  | all_except_option (s, x::xs) = 
    if same_string (s, x) then SOME (xs)
    else case all_except_option(s, xs) of
	     NONE => NONE 
	   | SOME y => SOME (x::y)
(* 1.b *)
fun get_substitutions1 ([], str) = []
  | get_substitutions1 (x::xs, str) =
    case all_except_option(str, x) of
	NONE => get_substitutions1(xs, str)
      | SOME y => y @ get_substitutions1(xs, str)

(* 1.c *)
(* tail recursion *)
fun get_substitutions2 (lst, str) =
    let
	fun get_substitutions_helper ([], str, acc) = acc
	  | get_substitutions_helper (x::xs, str, acc) =
	    case all_except_option(str, x) of
		NONE => get_substitutions_helper(xs, str, acc)
	      | SOME y => get_substitutions_helper(xs, str, acc @ y)
    in
	get_substitutions_helper(lst, str, [])
    end

(* 1.d *)
fun similar_names (lst, name: {first: string, middle: string, last: string}) =
    let
	fun construct_names ([], mid, l) = []
	  | construct_names (z::zs, mid, l) =
	    [{first = z, last = l, middle = mid}] @ construct_names(zs, mid, l)
    in
	[name] @ construct_names(get_substitutions1(lst, #first name), #middle name, #last name)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2.a *)
fun card_color (Clubs, _) = Black
  | card_color (Spades, _) = Black
  | card_color (_, _) = Red

(* 2.b *)
fun card_value (_, Num x) = x
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10

(* 2.c *)
fun remove_card ([], c: card, e) = raise e
  | remove_card (x::xs, c, e) =
    if (x = c) then xs else x::remove_card(xs, c, e)
                                          
(* 2.d *)                        
fun all_same_color ([]) = false
  | all_same_color (x::xs) =
    case xs of
        [] => true
      | y::ys => case card_color(x) = card_color(y) of
                     true => all_same_color(xs)
                   | false => false

(* 2.e*)
fun sum_cards ([]) = 0
  | sum_cards (x::xs) =
    let
        fun sum_cards_h ([], acc) = acc
          | sum_cards_h (y::ys, acc) = sum_cards_h(ys, card_value(y) + acc)
    in
        sum_cards_h(xs, card_value(x))
    end

(* 2.f *)
fun score (cs, g) =
    (if sum_cards(cs) > g then (sum_cards(cs) - g)*3
     else g - sum_cards(cs)) div (if all_same_color(cs) then 2 else 1)
                                                  
fun officiate_core (cards, moves, goal, f) =
    let
        fun cards_game (_, [], hs, g) = f(hs, g)
          | cards_game (cs, m::mx, hs, g) =
            case m of
                Draw => (case cs of
                             c::cx => if sum_cards(c::hs) > g then f(c::hs, g) else cards_game(cx, mx, c::hs, g)
                           | [] => cards_game([], mx, hs, g))
              | Discard y => case remove_card(hs, y, IllegalMove) of
                                 [] => f([], g)
                               | nhs => cards_game(cs, mx, nhs, g)
    in
        cards_game(cards, moves, [], goal)
    end    

(* 2.g *)
fun officiate (cards, moves, goal) =
    officiate_core (cards, moves, goal, score)

fun score_challenge (cs, g) =
    let
        fun score_l (t, g) = if t > g then (t - g)*3 else g - t

        fun score_s ([], t, g) = score_l(t, g)
          | score_s (x::xs, t, g) =
            case card_value(x) of
                11 => if (score_s(xs, 1 + t, g) < score_s(xs, 11 + t, g)) then score_s(xs, 1 + t, g) else score_s(xs, 11 + t, g)
              | _ => score_s(xs, card_value(x) + t, g)
    in
        (score_s (cs, 0, g)) div (if all_same_color(cs) then 2 else 1)
    end

fun officiate_challenge (cards, moves, goal) =
    officiate_core (cards, moves, goal, score_challenge)
    
fun careful_player (cards, goal) =
    let
        fun skipList ([], a, i) = a
          | skipList (x::xs, a, i) = skipList (xs, if i <> 0 then a@[x] else a, i - 1)
                                              
        fun findIfRemove (cs: (suit * rank) list, n, g) =
            if (n < 0) then NONE else
            if score(skipList(cs,[], n), g) = 0
            then SOME n
            else findIfRemove(cs, n-1, g)

        fun findIfRemoveL (cs: (suit * rank) list, g) =
            findIfRemove(cs, length(cs)-1, g)
                        
        fun getCardToDiscard (x::xs, y) =
            if y = 0 then Discard x
            else getCardToDiscard(xs, y-1)
                 
        fun cal ([], hs, ac, g) = ac
          | cal (x::xs, hs, ac, g) =
            if sum_cards(x::hs) > g then
                case findIfRemoveL(x::hs, g) of
                    NONE => ac
                  | SOME y => ac@[Draw]@[getCardToDiscard(x::hs, y)]
            else cal (xs, x::hs, ac@[Draw], g) 
    in
        cal(cards, [], [], goal)
    end
        
