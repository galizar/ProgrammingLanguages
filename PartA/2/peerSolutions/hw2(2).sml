(* auxiliar function *)
fun same_string (s1: string, s2: string) =
    s1 = s2

(* 1.a. Takes a string and a string list. Returns NONE if the string is not *)
(* in the list, or SOME lst where lst is the argument list except the string *)
(* is not in it. *)
fun all_except_option (x, xs) =
    let fun all_except_option (_, []) = NONE
	  | all_except_option (acc, x'::xs') =
	    if same_string(x, x')
	    then SOME(acc @ xs')
	    else all_except_option(acc @ [x'], xs')
    in
	all_except_option ([], xs)
    end

(* 1.b. Takes a string list list and a string s and returns a string list. *)
fun get_substitutions1 ([], x) = []
  | get_substitutions1 (xs::xs', x) =
    case all_except_option(x, xs) of
	NONE => get_substitutions1(xs', x)
      | SOME s => s @ get_substitutions1(xs', x)

(* 1.c. Takes a string list list and a string s and returns a string list. *)
(* Tail-recursive version. *)
fun get_substitutions2 ([], x) = []
  | get_substitutions2 (xs::xs', x) =
  let fun append ([], ss) = ss
    | append (xs::xs', ss) =
    case all_except_option(x, xs) of
         NONE => append(xs', ss)
       | SOME ss' => append(xs', ss @ ss')
    in
      append (xs::xs', [])
    end

(* 1.d. Takes a string list list of substitutions and a full name of type *)
(* {first:string,middle:string,last:string} and returns a list of full names. *)
fun similar_names ([], {first: string, last: string, middle: string}) =
    [{first = first, last = last , middle = middle}]
  | similar_names (x::xs, {first: string, last: string, middle: string}) =
    let val subs =  get_substitutions2(x::xs, first)
	fun replace ([]) = []
	  | replace (y::ys) =
	    {first = y, last = last, middle = middle} :: replace(ys)
    in
	{first = first, last = last , middle = middle} :: replace(subs)
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* 2.a. Takes a card and returns its color. *)
fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _) = Red

(* 2.b. Takes a card and returns its value. *)
fun card_value (_, (Num x)) = x
  | card_value (_, Ace) = 11
  | card_value (_, _) = 10

(* 2.c. Takes a list of cards cs, a card c, and an exception e. It returns a *)
(* list that has all the elements of cs except c. *)
fun remove_card (cs: card list, c: card, e) =
    let fun remove_card (_, []) = NONE
	  | remove_card (acc, c'::cs') = if c = c'
					 then SOME(acc @ cs')
					 else remove_card(acc @ [c'], cs')
    in
	case remove_card([], cs) of
	    NONE => raise e
	  | SOME cs => cs
    end

(* 2.d. Takes a list of cards and returns true if all the cards in the list *)
(* are the same color. *)
fun all_same_color ([]) = true
  | all_same_color (_::[]) = true
  | all_same_color (c::(c'::cs)) =
    (card_color (c) = card_color(c')) andalso all_same_color(c'::cs)

(* 2.e. Takes a list of cards and returns the sum of their values. Tail *)
(* recursive. *)
fun sum_cards (cards) =
    let fun sum ([], acc) = acc
	  | sum (card::cards, acc) = sum(cards, card_value(card) + acc)
    in
	sum(cards, 0)
    end

(* 2.f. Takes a card list (the held-cards) and an int (the goal) and computes *)
(* the score. *)
fun score (cards, goal) =
    let val sum = sum_cards (cards)
	val pre_score =
	    if (sum > goal) then 3 * (sum - goal)
	    else goal - sum
    in
	if all_same_color(cards) then (pre_score div 2)
	else pre_score
    end

(* 2.g. Runs a game. It takes a card list, a move list and an int (the goal), *)
(* and returns the score at the end of the game after processing the moves in *)
(* the move list in order. *)
fun officiate (card_list, moves, goal) =
    let fun game_over held_cards =
	    score (held_cards, goal)
	fun run (_, [], held_cards) = game_over (held_cards)
	  | run (card_list, (Discard card)::moves, held_cards) =
	    run(card_list, moves, remove_card(held_cards, card, IllegalMove))
	  | run ([], Draw::moves, held_cards) = game_over (held_cards)
	  | run (card::card_list, Draw::moves, held_cards) =
	    if sum_cards (card::held_cards) > goal then
		game_over (card::held_cards)
	    else
		run (card_list, moves, card::held_cards)
    in
	run(card_list, moves, [])
    end
