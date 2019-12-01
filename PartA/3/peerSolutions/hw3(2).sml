(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs =
	List.filter(fn x => Char.isUpper(String.sub(x, 0)))(xs)

fun longest_string1 xs =
	List.foldl(fn(acc, x) => if (String.size x >= String.size acc) then x else acc) "" xs

fun longest_string2 xs =
	List.foldl(fn(acc, x) => if (String.size x > String.size acc) then x else acc) "" xs

fun longest_string_helper (f) =
	List.foldl(fn(acc, x) => if (f(String.size x, String.size acc)) then x else acc) ""

val longest_string3 = longest_string_helper(fn (x, y) => x >= y)
val longest_string4 = longest_string_helper(fn (x, y)  => x > y)


fun longest_capitalized xs =
	let val f = longest_string1 o only_capitals
	in f(xs)
	end

fun rev_string s =
	String.implode(List.rev(String.explode(s)))

fun first_answer f = fn xs =>
	case xs of
		[] => raise NoAnswer
		|x::xs' => case f(x) of
			SOME i => i
			| NONE => first_answer f xs'

fun all_answers f = fn ys =>
	let fun helper(acc, xs) =
		case xs of
			[] => acc
			|x::xs' => case f(x) of
				SOME i => helper(acc @ i, xs')
				| NONE => raise NoAnswer
	in
		SOME(helper([], ys)) handle NoAnswer => NONE
	end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards(p: pattern) =
	g(fn() => 1)(fn x => 0)(p)

fun count_wild_and_variable_lengths(p: pattern) =
	g(fn() => 1)(fn x => 1)(p)

fun count_some_var(s: string, p: pattern) =
	g(fn() => 0)(fn x => if x = s then 1  else 0)(p)

fun check_pat(p) =
	let
		fun all_strings p =
			case p of
				Variable x        => [x]
				| TupleP ps         => List.foldl (fn (p, i) => (all_strings p) @ i) [] ps
				| ConstructorP(_,p) => all_strings(p)
				| _                 => []
		fun repeats xs =
			case xs of
				[] => false
				| x::xs' =>
					if List.exists (fn y => x = y) xs'
					then true
					else repeats xs'
	in
		not(repeats(all_strings(p)))
	end

fun match(v, p) =
	case (v, p) of
	 (_, Wildcard) => SOME []
	| (sv, Variable sp) => SOME [(sp, sv)]
	| (Unit, UnitP) => SOME []
    | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
    | (Tuple tv, TupleP tp) => if List.length tv = List.length tp
                               then all_answers match (ListPair.zip(tv, tp))
                               else NONE
    | (Constructor (s1,cv), ConstructorP (s2,cp)) => if s1 = s2
                                                     then match (cv,cp)
                                                     else NONE
    | (_, _) => NONE


fun first_match v p_lst =
  SOME (first_answer (fn x => match(v, x)) p_lst) handle NoAnswer => NONE
