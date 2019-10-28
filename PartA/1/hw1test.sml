use "hw1.sml";
(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* === is_older === *)
val test_is_older1 = is_older((0001, 02, 03), (0002, 03, 04)) = true (*d1 is earlier cuz year*)
val test_is_older2 = is_older((0002, 03, 04), (0001, 02, 03)) = false (* d1 is later cuz year*)
val test_is_older3 = is_older((0002, 04, 06), (0002, 07, 02)) = true (* d1 is earlier cuz month*)
val test_is_older4 = is_older((2034, 09, 14), (2034, 02, 13)) = false (* d1 is later cuz month *)
val test_is_older5 = is_older((2077, 02, 06), (2077, 02, 22)) = true (* d1 is earlier cuz day *)
val test_is_older6 = is_older((2022, 11, 21), (2022, 11, 12)) = false(* d1 is later cuz day *)
val test_is_older7 = is_older((0001, 02, 03), (0001, 02, 03)) = false (* false cuz same dates *)

(* === number_in_month *)
val test_number_in_month1 = number_in_month([(2012, 2, 28),(2013, 12, 1)], 2) = 1
val test_number_in_month2 = number_in_month([], 3) = 0
val test_number_in_month3 = number_in_month([(2054, 03, 12), (2034, 05, 29), (2011, 07, 22),
                                             (2033, 05, 11), (2001, 05, 24)], 5)
                          = 3
val test_number_in_month4 = number_in_month([(2054, 03, 12), (2034, 05, 29), (2011, 07, 22)], 10)
                          = 0

(* === number_in_months === *)
val test_number_in_months1 =
  number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], []) = 0

val test_number_in_months2 =
  number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test_number_in_months3 =
  number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], [07, 09]) = 0


(* === dates_in_month === *)
val test_dates_in_month1 = dates_in_month([], 04)
                         = []
val test_dates_in_month2 = dates_in_month([(2012,2,28),(2013,12,1)],2)
                         = [(2012,2,28)]
val test_dates_in_month3 = dates_in_month([(2011, 07, 23), (2034, 03, 22), (1922, 07, 12)], 07)
                         = [(2011, 07, 23), (1922, 07, 12)]
val test_dates_in_month4 = dates_in_month([(2011, 07, 23), (2034, 03, 22), (1922, 07, 12)], 05)
                         = []

(* === dates_in_months === *)
val test_dates_in_months1 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], [])
                          = []
val test_dates_in_months2 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
                          = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test_dates_in_months3 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
                                            [1, 5, 7])
                          = []

(* === get_nth === *)
val test_get_nth1 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
(* how to test exceptions? *)

(* === date_to_string === *)
val test_date_to_string1 = date_to_string(2013, 6, 1) = "June 1, 2013"

(* === number_before_reaching_sum === *)
val number_before_reaching_sum1 = number_before_reaching_sum(0, [1, 2, 3]) = 0
val number_before_reaching_sum2 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3

(* === what_month === *)
val test_what_month1 = what_month 70 = 3
val test_what_month2 = what_month 15 = 1
val test_what_month3 = what_month 340 = 12

(* === month_range === *)
val test_month_range1 = month_range (20, 20) = [1]
val test_month_range2 = month_range (31, 34) = [1,2,2,2]

(* === oldest === *)
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
