fun is_older(a1: (int * int * int), a2: (int * int * int)) =
	if #1 a1 < #1 a2
	then true
	else if (#1 a1 = #1 a2) andalso (#2 a1 < #2 a2)
	then true
	else if (#1 a1 = #1 a2) andalso (#2 a1 = #2 a2) andalso (#3 a1 < #3 a2)
	then true
	else false

fun number_in_month (l: (int * int * int) list, n: int) =
	if null l
	then 0
	else if #2(hd l) = n
		then 1 + number_in_month(tl l,n)
		else number_in_month(tl l,n)

fun number_in_months (l: (int * int * int) list, nl: int list) =
	if null nl
	then 0
	else number_in_month(l, hd nl) + number_in_months(l, tl nl)

fun dates_in_month (l: (int * int * int) list, n: int) =
	if null l
	then []
	else if #2(hd l) = n
		then hd l :: dates_in_month(tl l, n)
		else dates_in_month(tl l, n)

fun dates_in_months (l: (int * int * int) list, nl: int list) =
	if null nl
	then []
	else dates_in_month(l, hd nl) @ dates_in_months(l, tl nl)

fun get_nth (l: string list, n: int) =
	if n = 1
	then hd l
	else get_nth(tl l, n - 1)

fun date_to_string (d: (int * int * int)) =
	let
		val months = ["January", "February", "March", "April", "May", "June", "August", "September", "October", "November", "December"]
	in
		get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
	end

fun number_before_reaching_sum (sum: int, l: int list) =
	if hd l >= sum
	then 0
	else 1 + number_before_reaching_sum(sum - hd l, tl l)

fun what_month (d: int) =
	let
		val months_len = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		1 + number_before_reaching_sum(d, months_len)
	end

fun month_range (day1: int, day2: int) =
	if day1 > day2
	then []
	else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (l: (int * int * int) list) =
	if null l
	then NONE
	else
		let val oldest_ans = oldest(tl l)
		in
			if null (tl l)
			then SOME (hd l)
			else if is_older(hd l, valOf (oldest_ans))
				then SOME (hd l)
				else oldest_ans
		end

(* TODO: do challenge problems if time makes it possible *)
