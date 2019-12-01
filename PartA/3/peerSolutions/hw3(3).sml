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

(* 1.  Write a functiononly_capitalsthat takes astring listand returns astring listthat has onlythe strings in the argument that start with an uppercase letter.  Assume all strings have at least 1character.  UseList.filter,Char.isUpper, andString.subto make a 1-2 line solution.*)

fun only_capitals(sl) =
    List.filter (fn e => Char.isUpper(String.sub(e, 0))) sl

(*2.  Write a function longest_string1that takes a string list and returns the longest string in the list.  If the list is empty, return "".  In the case of a tie, return the string closest to the beginning of thelist.  Usefoldl,String.size, and no recursion (other than the implementation offoldlis recursive)*)

fun longest_string1(sl) =
    List.foldl (fn (s, longestString) => if String.size(s) > String.size(longestString) then s else longestString) "" sl

(* 3.  Write  a  functionlongest_string2that  is  exactly  likelongest_string1except  in  the  case  of  tiesit returns the string closest to the end of the list.  Your solution should be almost an exact copy oflongest_string1.  Still usefoldlandString.size.*)

fun longest_string2(sl) =
    List.foldl (fn (s, longestString) => if String.size(s) >= String.size(longestString) then s else longestString) "" sl

(*
4.  Write functions longest_string_helper,longest_string3, and longest_string4such that:
•longest_string3 has  the  same  behavior  aslongest_string1 and longest_string4 has the same behavior as longest_string2.
•longest_string_helper has type(int * int -> bool) -> string list -> string(notice the currying).  This function will look a lot likelongest_string1andlongest_string2but is more general because it takes a function as an argument.
•Iflongest_string_helperis passed a function that behaves like>(so it returnstrueexactlywhen its first argument is stricly greater than its second), then the function returned has the samebehavior aslongest_string1.
•longest_string3andlongest_string4are defined withval-bindings and partial applicationsoflongest_string_helper.
*)

fun longest_string_helper f = fn sl =>
    List.foldl (fn (s, acc) => if f(String.size(s),String.size(acc)) then s else acc) "" sl

val longest_string3 = longest_string_helper (fn (a,b) => a > b)
val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

(*
5.  Write a function longest_capitalized that takes a string list and returns the longest string in the list that begins with an uppercase letter,  or "" if there are no such strings.  Assume all strings have at least 1 character.  Use a val-binding and the ML library’s o operator for composing functions. Resolve ties like in problem 2.
*)

val longest_capitalized = longest_string1 o only_capitals

(*
6.  Write a function rev_string that takes astringand returns thestringthat is the same characters inreverse order. Use ML’sooperator, the library functionrevfor reversing lists, and two library functionsin theStringmodule.  (Browse the module documentation to find the most useful functions.) *)

val rev_string = String.implode o List.rev o String.explode

(*
7.  Write a function first_answer of type (’a -> ’b  option) -> ’a list -> ’b (notice the 2 arguments are curried).  The first argument should be applied to elements of the second argument in order until the first time it returns SOME vfor some v and then v is the result of the call to first_answer.If the first argument returnsNONEfor all list elements, thenfirst_answershould raise the exceptionNoAnswer.  Hints:  Sample solution is 5 lines and does nothing fancy.
*)

fun first_answer f l =
    case l of
    [] => raise NoAnswer
    |e::l => case f e of
                SOME v => v
                | _ => first_answer f l

(*
8.  Write  a  function all_answers of  type(’a -> ’b list option) -> ’a list -> ’b list option(notice the 2 arguments are curried).  The first argument should be applied to elements of the second argument.   If  it  returns NONE for  any  element,  then  the  result  for all_answers is NONE.  Else  the calls to the first argument will have produced SOME lst1,SOME lst2, ...SOME lstn and the result of all_answers is SOME lst where lst is lst1,lst2, ...,lstn appended together (order doesn’t matter).Hints:  The sample solution is 8 lines.  It uses a helper function with an accumulator and uses @.  Note all_answers f []should evaluate toSOME [].
*)

fun all_answers f l =
    case l of
    [] => SOME []
    |e::l => case f e of
                NONE => NONE
                |SOME lst => case all_answers f l of
                                NONE => NONE
                                | SOME lst_all => SOME( lst @ lst_all)


(* 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x))

(* 9c *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* 10 Write  a  function check_pat that  takes  a  pattern  and  returns  true  if  and  only  if  all  the  variablesappearing  in  the  pattern  are  distinct  from  each  other*)
fun check_pat (p) =
    let
        fun list_variables p =
            case p of
                Variable v => [v]
                |TupleP pl => List.foldl (fn (p,acc) => list_variables p @ acc) [] pl
                | ConstructorP (_,p) => list_variables p
                | _ => []
        fun has_double l =
            case l of
                [] => false
                |e::[] => false
                |e1::e2::l => e1 = e2 orelse List.exists (fn e => e1 = e) l
    in
        not ((has_double o list_variables) p)
    end

(* 11 Write a function match that takes a valu * pattern and returns a(string * valu) list option,namelyNONEif the pattern does not match andSOME lstwherelstis the list of bindings if it does. *)

fun match (v, p) =
    case (v,p) of
        (_, Wildcard) => SOME []
        | (v, Variable s) => SOME [(s, v)]
        | (Unit, UnitP) => SOME []
        | (Const c1, ConstP c2) => if c1 = c2 then SOME [] else NONE
        | (Tuple [], TupleP []) => SOME []
        | (Tuple (v1::vl), TupleP (p1::pl)) => (case (match(v1, p1), match((Tuple vl), (TupleP pl))) of
                                            (SOME l1, SOME l2) => SOME (l1 @ l2)
                                            |_ => NONE)
        | (Constructor (s1,v), ConstructorP (s2,p)) => if s1 = s2 then match(v,p) else NONE
        | _ => NONE


(* 12 Write a functionfirst_matchthat takes a value and a list of patterns and returns a(string * valu) list option,  namelyNONEif  no  pattern  in  the  list  matches  orSOME lstwherelstis  the  list  of  bindings  for  the  first  pattern  in  the  list  that  matches. *)

fun first_match v lp =
    SOME (first_answer (fn p => match(v,p)) lp) handle NoAnswer => NONE
