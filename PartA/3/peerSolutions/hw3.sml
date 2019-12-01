(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


(* 1 *)
val only_capitals  = List.filter (fn s => Char.isUpper(String.sub(s, 0)))


(* 2 *)
val longest_string1 = List.foldl (fn (x,y) =>
				     if String.size x > String.size y
				     then x
				     else y) ""


(* 3 *)
val longest_string2 = List.foldl (fn (x,y) =>
				     if String.size x >= String.size y
				     then x
				     else y) ""


(* 4 *)

fun longest_string_helper f = List.foldl (fn (x,y) =>
					        if f(String.size x, String.size y)
						then x
						else y) ""

val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)


(* 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* 6 *)
val rev_string = String.implode o List.rev o String.explode


(* 7 *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v


(* 8 *)
fun all_answers f xs =
    let fun append_answers (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => case f x of
			      NONE => raise NoAnswer
			    | SOME lst => append_answers(xs', acc @ lst)
    in SOME(append_answers(xs, [])) handle NoAnswer => NONE
    end


(* 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0)


(* 9b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)


(* 9c *)
fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p


(* 10 *)
fun check_pat p =
    let fun all_variables p =
	    case p of
		Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p',lst) => lst @ (all_variables p')) [] ps
	      | ConstructorP(_,p') => all_variables p'
	      | _                 => []
	fun all_distinct (xs : string list) =
	    case xs of
		[] => true
	      | x::xs' => List.all (fn y => y <> x) xs' andalso all_distinct xs'
    in (all_distinct o all_variables) p
    end


(* 11 *)
fun match (v,p) =
    case (v,p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => (all_answers match (ListPair.zipEq (vs,ps)) handle
				  UnequalLengths => NONE)
      | (Constructor(s1,v'),ConstructorP(s2,p')) => if s1 = s2 then match(v',p') else NONE
      | _ => NONE


(* 12 *)
fun first_match v ps = SOME(first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE



