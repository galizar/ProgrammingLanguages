(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* === HELPERS === *)
fun reverse(l: 'a list) =
  let
    fun fn_for_list(original: 'a list, reversed: 'a list) =
      case original of
           [] => reversed
         | first::rest => fn_for_list(rest, first::reversed)
  in
    fn_for_list(l, [])
  end

(* === FUNCTIONS === *)
fun all_except_option(s: string, los0: string list) =
  let
    fun fn_for_los(los: string list, acc: string list) =
      case los of
           [] => NONE
         | str::strs' =>
             if same_string(str, s)
             then SOME ( reverse(acc) @ strs' )
             else fn_for_los(strs', str::acc)
  in
    fn_for_los(los0, [])
  end

fun get_substitutions1(substitutions: string list list, s: string) =
  case substitutions of
       [] => []
     | first::rest =>
         let
           val try = all_except_option(s, first)
           val next = get_substitutions1(rest, s)
         in
           case try of
                NONE => next
              | SOME [] => next
              | SOME first_without_s => first_without_s::next
         end

fun get_substitutions2(substitutions0: string list list, s: string) =
  let
    fun aux(substitutions: string list list, acc: string list list) =
      case substitutions of
           [] => reverse acc
         | first::rest =>
             let
               val try = all_except_option(s, first)
             in
               case try of
                    NONE => aux(rest, acc)
                  | SOME [] => aux(rest, acc)
                  | SOME first_without_s => aux(rest, first_without_s::acc)
              end
  in
    aux(substitutions0, [])
  end

type full_name = {first: string, middle: string, last: string}

fun similar_names(candidates: string list list, name: full_name) =
  let
    fun replace_first_with(replacement: string) =
      {first = replacement, middle = #middle name, last = #last name}

    fun fn_for_sl(substitutes: string list, acc: full_name list) =
      case substitutes of
           [] => acc
         | str::strs' => fn_for_sl(strs', replace_first_with(str) :: acc)

    fun fn_for_sll(substitutions: string list list, acc: full_name list) =
      case substitutions of
           [] => reverse acc
         | sl::slls' => fn_for_sll(slls', fn_for_sl(sl, acc))

  in
    fn_for_sll(get_substitutions2(candidates, #first name), [name])
  end

fun card_color(c: card) =
  case c of
       (Spades, _) => Black
     | (Clubs, _) => Black
     | _ => Red

fun card_value (c: card) =
  case c of
       (_, Num n) => n
     | (_, Ace) => 11
     |  _ => 10

fun remove_card (cs: card list, c: card, e: exn) =
  case cs of
       [] => raise e
     | fc::cs' => if fc = c then cs' else fc::remove_card(cs', c, e)

fun all_same_color (l: card list) =
    case l of
         [] => true
       | c1::[] => true
       | c1::c2::cs' =>
           if card_color(c1) = card_color(c2)
           then all_same_color(c2::cs')
           else false

fun sum_cards (l: card list) =
  let
    fun fn_for_card_list (x: card list, sum: int) =
      case x of
           [] => sum
         | c1::cs' => fn_for_card_list(cs', sum + card_value(c1))
  in
    fn_for_card_list(l, 0)
  end

fun score (l: card list, goal: int) =
  let
    val sum = sum_cards(l)
    val diff = abs (sum - goal)
    val preliminary = if sum > goal then 3 * diff else diff
  in
    if all_same_color(l) andalso preliminary > 0
    then preliminary div 2
    else preliminary
  end

fun officiate (cards: card list, moves: move list, goal: int) =
  let
    fun make_moves (ms: move list, avail: card list, held: card list) =
      let
        fun draw_card (next_moves: move list) =
          case avail of
               [] => score (held, goal)
             | c1::cs' => if sum_cards (c1::held) > goal
                          then score (c1::held, goal)
                          else make_moves (next_moves, cs', c1::held)
      in
        case ms of
             [] => score (held, goal)
           | Discard c::ms' => make_moves (ms', avail, remove_card (held, c, IllegalMove))
           | _::ms' => draw_card (ms')
      end
  in
    make_moves (moves, cards, [])
  end
