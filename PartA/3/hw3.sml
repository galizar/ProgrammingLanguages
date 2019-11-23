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

fun only_capitals (l: string list) = List.filter (fn x => Char.isUpper(String.sub(x, 0))) l

fun longest_string1 (l: string list) =
  foldl (fn (a, b) => if String.size (a) > String.size (b) then a else b) "" l

fun longest_string2 (l: string list) =
  foldl (fn (a, b) => if String.size (a) >= String.size (b) then a else b) "" l

fun longest_string_helper op o l =
  foldl (fn (a, b) => if String.size (a) o String.size (b) then a else b) "" l

fun longest_string3 (l: string list) = longest_string_helper op > l

fun longest_string4 (l: string list) = longest_string_helper op >= l

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f l =
  case l of
       [] => raise NoAnswer
     | first::rest => case f first of
                           NONE => first_answer f rest
                         | SOME v => v

fun all_answers f l0 =
  let
    fun aux l acc =
      case l of
           [] => SOME (rev acc)
         | first::rest =>
             case f first of
                  NONE => NONE
                | SOME v => aux (rest) (v @ acc)
  in
    aux l0 []
  end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

fun count_some_var (s: string, p: pattern) =
  g (fn _ => 0) (fn vname => if s = vname then 1 else 0) p

fun check_pat p0 =
  let
    fun all_vnames (p: pattern, acc: string list) =
      case p of
           Variable name => name::acc
         | ConstructorP (_, nestedP) => all_vnames (nestedP, acc)
         | TupleP ps => foldl (fn (prn, sofar) => sofar @ (all_vnames (prn, []))) [] ps
         | _ => acc

     fun any_dups (strs: string list)  =
       case strs of
            [] => false
          | s::ss' => if List.exists (fn x => x = s) ss'
                      then true
                      else any_dups (ss')
  in
    not (any_dups (all_vnames (p0, [])))
  end

fun match (pair: (valu * pattern)) =
  case pair of
       (_, Wildcard) => SOME []
     | (v, Variable name) => SOME [(name, v)]
     | (v, UnitP) => if v = Unit then SOME [] else NONE
     | (Const v, ConstP vp) => if v = vp then SOME [] else NONE
     | (Tuple vs, TupleP ps) =>
         if check_pat ps
         then
           let
             val paired = zip (vs, ps)
             val result = foldl (fn (pair, sofar) => (match pair) @ sofar) []
