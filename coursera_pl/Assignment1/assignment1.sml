(*
Sam Caldwell
Assignment 1
Coursera Programming Languages 
*)

fun get_year(date : int*int*int) = #1 date
fun get_month(date : int*int*int) = #2 date
fun get_day(date: int*int*int) = #3 date

fun is_older(a : int*int*int, b : int*int*int) =
    let val year_a = get_year a
        val year_b = get_year b
        val month_a = get_month a
	val month_b = get_month b
	val day_a   = get_day a
        val day_b   = get_day b
    in year_a < year_b orelse (year_a = year_b andalso
			       (month_a < month_b orelse (month_a = month_b andalso
							 day_a < day_b)))
    end

fun test_is_older() =
    let val z = (0,1,1)
        val a = not(is_older(z,z))
        val b = is_older(z, (0,1,2))
        val c = is_older((0,0,1), (0,0,2))
        val d = not (is_older((0,0,2), (0,0,1)))
        val e = is_older((1,11,364), (2,0,0))
	val f = is_older((1,0,364), (1,1,0))
	val g = is_older((1,2,3), (1,2,4))
    in
	a andalso b andalso c andalso d andalso e andalso f andalso g
    end

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else let val date = hd dates
             val rest = tl dates
             val in_rest = number_in_month(rest, month)
         in if get_month date = month
            then 1 + in_rest
            else in_rest
         end

fun test_number_in_month() =
    let val a = 0 = number_in_month([], 1)
        val b = 1 = number_in_month([(1,1,1)], 1)
        val c = 1 = number_in_month([(1,1,1), (1,2,1)], 1)
        val d = 2 = number_in_month([(1,1,1), (1,2,1), (3,1,45)], 1)
    in
        a andalso b andalso c andalso d
    end

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else let val month = hd months
             val rest = tl months
         in
             if number_in_month(dates, month) > 0
             then 1 + number_in_months(dates, rest)
             else number_in_months(dates, rest)
         end

val test_number_in_months =
    let val dates = []
        val months = []
        val a = 0 = number_in_months(dates, months)
        val dates' = (1,1,1) :: dates
        val b = 0 = number_in_months(dates', months)
        val months' = 2 :: months
        val c = 0 = number_in_months(dates, months')
        val months'' = 1 :: months'
        val d = 1 = number_in_months(dates', months'')
        val dates'' = (2000, 2, 13) :: dates'
        val e = 2 = number_in_months(dates'', months'')
        val dates''' = (2000, 4, 14) :: dates''
        val f = 2 = number_in_months(dates''', months'')
    in
        a andalso b andalso c andalso d andalso e andalso f
    end

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else let val date = hd dates
             val dates' = tl dates
         in
             if month = get_month date
             then date :: dates_in_month(dates', month)
             else dates_in_month(dates', month)
         end

val test_dates_in_month = 
    let val dates = []
        val a = [] = dates_in_month(dates, 1)
        val b = [] = dates_in_month(dates, 8)
        val dates' = (1,1,1) :: dates
        val c = [(1,1,1)] = dates_in_month(dates', 1)
        val d = [] = dates_in_month(dates', 2)
        val dates'' = (42, 2, 18) :: dates'
        val e = [(1,1,1)] = dates_in_month(dates'', 1)
        val f = [(42,2,18)] = dates_in_month(dates'', 2)
        val g = [] = dates_in_month(dates'', 9)
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g
    end

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else let val month = hd months
             val months' = tl months
         in
             dates_in_month(dates, month) @ dates_in_months(dates, months')
         end

val test_dates_in_months =
    let val dates = []
        val months = []
        val a = [] = dates_in_months(dates, months)
        val dates' = (1,1,1) :: dates
        val months' = 1 :: months
        val b = [] = dates_in_months(dates, months')
        val c = [] = dates_in_months(dates', months)
        val d = [(1,1,1)] = dates_in_months(dates', months')
        val months'' = 2 :: months'
        val e = [(1,1,1)] = dates_in_months(dates', months'')
        val dates'' = (3,2,1) :: dates'
        val f = [(3,2,1), (1,1,1)] = dates_in_months(dates'', months'')
    in
        a andalso b andalso c andalso d andalso e andalso f
    end

fun get_nth(xs : string list, n : int) =
    if n > 1
    then get_nth(tl xs, n - 1)
    else hd xs

val test_get_nth =
    let val l = ["foo"]
        val a = "foo" = get_nth(l, 1)
        val l' = "bar" :: l
        val b = "bar" = get_nth(l', 1)
        val c = "foo" = get_nth(l', 2)
    in
        a andalso b andalso c
    end

fun date_to_string(date : int*int*int) =
    let val dates = ["January", "February", "March", "April", "May", "June", "July",
                     "August", "September", "October", "November", "December"]
        val year = get_year date
        val month = get_month date
        val day   = get_day date
    in
        get_nth(dates, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
    end

val test_date_to_string =
    let val a = "January 20, 2013" = date_to_string(2013,1,20)
        val b = "July 16, 42" = date_to_string(42, 7, 16)
        val c = "October 31, 1492" = date_to_string(1492, 10, 31)
    in
        a andalso b andalso c
    end
