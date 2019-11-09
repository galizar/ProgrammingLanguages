use "hw2.sml";

val test_all_except_option0 = all_except_option("culo", []) = NONE
val test_all_except_option1 = all_except_option("string", ["string"]) = SOME []
val test_all_except_option2 = all_except_option("culo", ["culo", "hola", "ciao"]) = SOME ["hola", "ciao"]
val test_all_except_option3 = all_except_option("culo", ["hola", "culo", "ciao"]) = SOME ["hola", "ciao"]
val test_all_except_option4 = all_except_option("culo", ["hola", "ciao", "culo"]) = SOME ["hola", "ciao"]
val test_all_except_option5 = all_except_option("culo", ["hola", "ciao", "arrivederci"]) = NONE

val test_get_substitutions11 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_get_substitutions12 = get_substitutions1([["arrivederci", "adios", "ciao"], ["hola", "hallo"]], "ciao")
                             = [["arrivederci", "adios"]]
val test_get_substitutions13 = get_substitutions1([["arrivederci", "adios", "ciao"],
                                                  ["hola", "hallo"]], "culo")
                             = []
val test_get_substitutions14 = get_substitutions1([["arrivederci", "adios", "ciao", "culo"],
                                                  ["hola", "hallo"],
                                                  ["test", "prueba", "random", "culo"]], "culo")
                             = [["arrivederci", "adios", "ciao"], ["test", "prueba", "random"]]

val test_get_substitutions21 = get_substitutions2([["foo"],["there"]], "foo") = []
val test_get_substitutions22 = get_substitutions2([["arrivederci", "adios", "ciao"], ["hola", "hallo"]], "ciao")
                             = [["arrivederci", "adios"]]
val test_get_substitutions23 = get_substitutions2([["arrivederci", "adios", "ciao"],
                                                  ["hola", "hallo"]], "culo")
                             = []
val test_get_substitutions24 = get_substitutions2([["arrivederci", "adios", "ciao", "culo"],
                                                  ["hola", "hallo"],
                                                  ["test", "prueba", "random", "culo"]], "culo")
                             = [["arrivederci", "adios", "ciao"], ["test", "prueba", "random"]]

val test_similar_names0 = similar_names([], {first="Toribio", middle="Petronilo", last="Furier"})
                        = [{first="Toribio", middle="Petronilo", last="Furier"}]

val test_similar_names1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                                         {first="Fred", middle="W", last="Smith"}) =
  [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
    {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test_similar_names2 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                                         {first="Toribio", middle="W", last="Smith"})
                        = [{first="Toribio", middle="W", last="Smith"}]


val test_card_color1 = card_color (Clubs, Num 2) = Black
val test_card_color2 = card_color (Diamonds, Num 3) = Red
val test_card_color3 = card_color (Spades, Num 2) = Black
val test_card_color4 = card_color (Hearts, Num 2) = Red

val test_card_value1 = card_value (Clubs, Num 2) = 2
val test_card_value2 = card_value (Diamonds, Ace) = 11
val test_card_value3 = card_value (Spades, Queen) = 10

val test_remove_card1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

(*

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
 [Draw,Draw,Draw,Draw,Draw],
  42)
  = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
 [Draw,Discard(Hearts,Jack)],
  42);
  false)
  handle IllegalMove => true)

*)
