use "hw3.sml";
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test_only_capitals1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test_only_capitals2 = only_capitals ["culo", "tetas", "ciao", "Arrivederci", "Sayonara", "pene"]
                        = ["Arrivederci", "Sayonara"]

val test_longest_string10 = longest_string1 [] = ""
val test_longest_string11 = longest_string1 ["A","bc","C"] = "bc"
val test_longest_string12 = longest_string1 ["culo", "hola"] = "culo"

val test_longest_string21 = longest_string2 ["A","bc","C"] = "bc"
val test_longest_string22 = longest_string2 ["hola", "ciao", "sayonara", "chaochao"] = "chaochao"
val test_longest_string23 = longest_string2 ["culo", "hola"] = "hola"

val test_longest_string30 = longest_string3 [] = ""
val test_longest_string31 = longest_string3 ["A","bc","C"] = "bc"
val test_longest_string32 = longest_string3 ["culo", "hola"] = "culo"

val test_longest_string41 = longest_string4 ["A","bc","C"] = "bc"
val test_longest_string42 = longest_string4 ["hola", "ciao", "sayonara", "chaochao"] = "chaochao"
val test_longest_string43 = longest_string4 ["culo", "hola"] = "hola"
val test_longest_string44 = longest_string4 ["A","B","C"] = "C"

val test_longest_capitalized1 = longest_capitalized ["A","bc","C"] = "A"
val test_longest_capitalized2 = longest_capitalized ["hola", "chao", "sayonara"] = ""
val test_longest_capitalized3 = longest_capitalized ["Infix", "cultured", "Swine"] = "Infix"
val test_longest_capitalized4 = longest_capitalized ["hola", "kek", "culin", "LoOol"] = "LoOol"

val test_rev_string0 = rev_string "" = ""
val test_rev_string1 = rev_string "abc" = "cba"

val test_first_answer1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test_all_answers0 = all_answers (fn x => x) [] = SOME []
val test_all_answers1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test_all_answers2 = all_answers (fn x => if x = 3 orelse x = 8 orelse x > 5 then SOME [x] else NONE) [11, 3, 7, 11, 12, 3, 8, 9, 3]
                      = SOME [11, 3, 7, 11, 12, 3, 8, 9, 3]


val test_count_wildcards1 = count_wildcards Wildcard = 1
val test_count_wildcards2 = count_wildcards (ConstructorP ("letest",
                                                          TupleP [Wildcard, ConstP 2, Variable "tralala"]))
                          = 1
val test_count_wildcards3 = count_wildcards (ConstructorP ("letest",
                                                           ConstructorP ("lalala",
                                                                         TupleP [Wildcard, UnitP,
                                                                         Wildcard])))
                          = 2

val test_blabla1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test_blabla2 = count_wild_and_variable_lengths (ConstructorP ("leletest",
                                                                  TupleP [Wildcard, Variable
                                                                  "Hello", UnitP]))
                 = 6
val test_blabla3 = count_wild_and_variable_lengths UnitP = 0


val test_count_some_var0 = count_some_var ("hello", UnitP) = 0
val test_count_some_var1 = count_some_var ("x", Variable("x")) = 1
val test_count_some_var2 = count_some_var ("foo", ConstructorP ("lelele", Variable "jojo")) = 0
val test_count_some_var3 = count_some_var ("baz", ConstructorP ("lolo",
                                                                TupleP [Wildcard, Variable "baz"]))
                         = 1
val test_count_some_var4 = count_some_var ("gus",
                                           ConstructorP ("jeje",
                                                         TupleP [UnitP,
                                                                 ConstP 3,
                                                                 ConstructorP ("haha", Variable "gus"),
                                                                 Variable "gus"]))
                         = 2



val test_check_pat1 = check_pat (Variable("x")) = true
val test_check_pat2 = check_pat (ConstructorP ("Culo", TupleP [UnitP, Variable "lel", Variable "lel"]))
                    = false
val test_check_pat3 = check_pat (ConstructorP ("lelel", UnitP)) = true
val test_check_pat4 = check_pat (ConstructorP ("jojo1", TupleP [ConstP 5,
                                                                ConstructorP ("sfsja", Variable "xD"),
                                                                Variable "xD"])) = false


val test_match1 = match (Const(1), UnitP) = NONE
val test_match2 = match (Unit, UnitP) = SOME []
val test_match3 = match (Tuple [Unit, Const 5], TupleP [UnitP, Variable "lel"])
                = SOME [("lel", Const 5)]

(*

val test12 = first_match Unit [UnitP] = SOME []
*)

