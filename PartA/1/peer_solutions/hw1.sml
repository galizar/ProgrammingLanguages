(* Homework 1: Tuples, Lists, Options, Recursion *)

fun is_older (d1 : (int * int * int), d2 : (int * int * int)) =
    let
	val year1 = #1 d1
	val month1 = #2 d1
	val day1 = #3 d1
	val year2 = #1 d2
	val month2 = #2 d2
	val day2 = #3 d2
    in
	year1 < year2 orelse
	(year1 = year2 andalso month1 < month2) orelse
	(year1 = year2 andalso month1 = month2 andalso day1 < day2)
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else (if (#2 (hd dates)) = month then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

val months = [
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
]

val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun date_to_string (date : (int * int * int)) =
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum (sum : int, xs : int list) =
    if sum <= hd xs
    then 0
    else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)

fun what_month (day : int) =
    1 + number_before_reaching_sum (day, days_in_months)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else (what_month day1)::month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let val oldest_tl = oldest(tl dates)
	 in
	     if isSome oldest_tl andalso is_older(valOf oldest_tl, hd dates)
	     then oldest_tl
	     else SOME (hd dates)
	 end

fun deduplicate (xs : int list) =
    let fun contains (xs : int list, x : int) =
	    not (null xs) andalso ((hd xs) = x orelse contains(tl xs, x))
    in
	if null xs
	then []
	else if contains(tl xs, hd xs)
	then deduplicate(tl xs)
	else (hd xs)::deduplicate(tl xs)
    end

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months(dates, deduplicate months)

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, deduplicate months)

fun reasonable_date (date : (int * int * int)) =
    let
	val year = #1 date;
	val month = #2 date;
	val day = #3 date;
	fun get_nth (xs : int list, n : int) =
	    if n = 1
	    then hd xs
	    else get_nth (tl xs, n - 1)
	fun is_leap (year : int) =
	    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    in
	year >= 1 andalso
	month >= 1 andalso
	month <= 12 andalso
	(day <= get_nth(days_in_months, month)
	 orelse (month = 2 andalso day = 29 andalso is_leap year))
    end
