(* === HELPERS === *)
fun reverse(l: 'a list) =
  let
    fun fn_for_list(original: 'a list, reversed: 'a list) =
      if null original
      then reversed
      else fn_for_list(tl original, hd original :: reversed)
  in
    fn_for_list(l, [])
  end

(* === DATA DEFINITIONS === *)

(* Date is (YYYY * MM * DD)
   with types (int * int * int) *)

val D1 = (2000, 03, 24)
val D2 = (1970, 12, 30)
val D3 = (2008, 02, 28)
val D4 = (2064, 02, 29) (* leap year *)

(* Day is int[1, 365] *)
(* Month is int[1, 12] *)

(* === CONSTANTS === *)

val MONTHS = ["January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December"]

val MONTH_LENGTHS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

(* === FUNCTIONS === *)

(* Date Date -> boolean *)
(* produces a boolean indicating whether d1 is before d2 *)
fun is_older(d1: int * int * int, d2: int * int * int) =
  if not (#1 d1 = #1 d2)
  then #1 d1 < #1 d2
  else
    if not (#2 d1 = #2 d2)
    then #2 d1 < #2 d2
    else #3 d1 < #3 d2

(* (Date list) int -> int *)
(* returns the number of dates in the list that are in the given month *)
fun number_in_month(lod0: (int * int * int) list, month: int) =
  let
    fun fn_for_lod(lod: (int * int * int) list, acc: int) =
      if null lod
      then acc
      else
        if #2 (hd lod) = month
        then fn_for_lod(tl lod, acc + 1)
        else fn_for_lod(tl lod, acc)
  in
    fn_for_lod(lod0, 0)
  end

(* (Date list) (int list) -> int *)
(* returns the number of dates that are in any month in the list of months
   IMPERATIVE: no duplicates in months *)
fun number_in_months(lod: (int * int * int) list, months: int list) =
  if null months
  then 0
  else
    number_in_month(lod, hd months) + number_in_months(lod, tl months)

(* (Date list) int -> (Date list) *)
(* returns a list of the dates that are in the given month*)
fun dates_in_month(lod0: (int * int * int) list, month: int) =
  (* in_month is a list of dates in the given month*)
  let
    fun fn_for_lod(lod: (int * int * int) list, in_month: (int * int * int) list) =
      if null lod
      then reverse in_month
      else
        if #2 (hd lod) = month
        then fn_for_lod(tl lod, hd lod :: in_month)
        else fn_for_lod(tl lod, in_month)
  in
    fn_for_lod(lod0, [])
  end


(* (Date list) (int list) -> (Date list) *) (* returns a list of the dates that are in any of the given months
   IMPERATIVE: no duplicates in months *)
fun dates_in_months(lod: (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(lod, hd months) @ dates_in_months(lod, tl months)

(* (string list) int -> String *)
(* returns the nth string in the list
   IMPERATIVE: length los0 > 0 and 1 <= n <= length los0
               throws error otherwise*)
fun get_nth(los0: string list, n: int) =
  let
    fun fn_for_los(los: string list, curr_idx: int) =
      if curr_idx = n
      then hd los
      else fn_for_los(tl los, curr_idx + 1)
  in
    fn_for_los(los0, 1)
  end

(* Date -> string *)
(* returns the string representation of the given date *)
fun date_to_string(date: (int * int * int)) =
  get_nth(MONTHS, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* int (int list) -> int*)
(* returns an int n such that the sum of the first n elements of loi0 is less than sum*)
(* IMPERATIVE: the sum of the elements in the list is at least the given int sum
               throws error otherwise *)
fun number_before_reaching_sum(sum: int, loi0: int list) =
  let
    fun fn_for_loi(loi: int list, sum_so_far: int, n_traversed: int) =
      if (sum_so_far + hd loi) >= sum
      then n_traversed
      else fn_for_loi(tl loi, sum_so_far + hd loi, n_traversed + 1)
  in
    fn_for_loi(loi0, 0, 0)
  end

(* Day -> int *)
(* returns the month that the given day is in *)
fun what_month(day: int) =
  number_before_reaching_sum(day, MONTH_LENGTHS) + 1

(* Day Day -> Month list *)
(* returns a list of the months each day between day1 and day2 is in
   IMPERATIVE: day1 <= day2 *)
fun month_range(day1: int, day2: int) =
  let
    fun fn_for_int(curr_day: int, last_day: int, months: int list) =
      if curr_day > last_day
      then reverse months
      else
        fn_for_int(curr_day + 1, last_day, what_month(curr_day) :: months)
  in
    fn_for_int(day1, day2, [])
  end

(* (Day list) -> (int * int * int) option *)
(* returns a date option of the oldest date in the list or NONE if the list empty*)
fun oldest(dates: (int * int * int) list) =
  (* fails to return the correct oldest date when that date is last in the dates list*)
  if null dates
  then NONE
  else
    let
      fun oldest_date(lod: (int * int * int) list, oldest: (int * int * int)) =
        if null (tl lod)
        then oldest (*crap solution: if is_older(hd lod, oldest) then hd lod else oldest*)
        else
          if is_older(hd lod, oldest)
          then oldest_date(tl lod, hd lod)
          else oldest_date(tl lod, oldest)
    in
      SOME (oldest_date(dates, hd dates))
    end
