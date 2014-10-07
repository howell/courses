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

val test_is_older =
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

val test_number_in_month =
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

fun get_nth'(xs, n) =
    if n > 1
    then get_nth'(tl xs, n - 1)
    else hd xs

fun get_nth(xs : string list, n : int) =
    get_nth'(xs, n)

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

fun number_before_reaching_sum(sum : int, nums : int list) =
    let fun go(acc : int, n : int, xs : int list) =
            let val x = hd xs
            in if acc + x >= sum
               then n
               else go (acc + x, n + 1, tl xs)
            end
    in
        go (0, 0, nums)
    end

val test_number_before_reaching_sum =
    let val l = [1]
        val a = 0 = number_before_reaching_sum(0, l)
        val l' = [1, 2, 3]
        val b = 0 = number_before_reaching_sum(1, l')
        val c = 1 = number_before_reaching_sum(2, l')
        val d = 2 = number_before_reaching_sum(4, l')
    in
        a andalso b andalso c andalso d
    end

fun what_month(day : int) =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, months)
    end

val test_what_month =
    let val a = 1 = what_month(1)
        val b = 1 = what_month(31)
        val c = 12 = what_month(364)
        val d = 12 = what_month(365)
        val e = 2 = what_month(31 + 28)
        val f = 3 = what_month(31 + 28 + 15)
        val g = 10 = what_month(365 - 31 - 30 -10)
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g
    end
       
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else let val month1 = what_month day1
             val month2 = what_month day2
             fun go(m) = if m = month2 then [m] else m :: go(m + 1)
         in
             go month1
         end
    
val test_month_range =
    let val a = [] = month_range(100, 50)
        val b = [1] = month_range(1, 31)
        val c = [1,2] = month_range(1, 32)
        val d = [1,2,3,4,5,6,7,8,9,10,11,12] = month_range(1, 365)
    in
        a andalso b andalso c andalso d
    end

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun oldest'(ds) = let val date = hd ds
                                   val rest = tl ds
                               in
                                   if null rest
                                   then date
                                   else let val old_rest = oldest'(rest)
                                        in
                                            if is_older(date, old_rest)
                                            then date
                                            else old_rest
                                        end
                               end
         in
             SOME(oldest' dates)
         end

val test_oldest =
    let val dates = []
        val a = NONE = oldest(dates)
        val dates' = (2,2,2) :: dates
        val b = SOME(2,2,2) = oldest(dates')
        val dates'' = (1,1,1) :: dates'
        val c = SOME(1,1,1) = oldest(dates'')
        val dates''' = (3,3,3) :: dates''
        val d = SOME(1,1,1) = oldest(dates''')
    in
        a andalso b andalso c andalso d
    end

fun nub(list) =
    if null list
    then []
    else let val x = hd list
             fun remove_x(xs) =
                 if null xs
                 then []
                 else let val x' = hd xs
                          val xs' = tl xs
                      in if x = x' then remove_x(xs') else x' :: remove_x(xs')
                      end
         in
             x :: nub(remove_x(tl list))
         end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
    number_in_months(dates, nub months)

val test_number_in_months_challenge =
    let val dates = []
        val months = []
        val a = 0 = number_in_months_challenge(dates, months)
        val dates' = (1,1,1) :: dates
        val b = 0 = number_in_months_challenge(dates', months)
        val months' = 2 :: 2 :: months
        val c = 0 = number_in_months_challenge(dates, months')
        val months'' = 1 :: 2 :: 1 :: months'
        val d = 1 = number_in_months_challenge(dates', months'')
        val dates'' = (2000, 2, 13) :: dates'
        val e = 2 = number_in_months_challenge(dates'', months'')
        val dates''' = (2000, 4, 14) :: dates''
        val f = 2 = number_in_months_challenge(dates''', months'')
    in
        a andalso b andalso c andalso d andalso e andalso f
    end

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, nub months)

val test_dates_in_months_challenge =
    let val dates = []
        val months = []
        val a = [] = dates_in_months_challenge(dates, months)
        val dates' = (1,1,1) :: dates
        val months' = 1 :: 1 :: months
        val b = [] = dates_in_months_challenge(dates, months')
        val c = [] = dates_in_months_challenge(dates', months)
        val d = [(1,1,1)] = dates_in_months_challenge(dates', months')
        val months'' = 2 :: 1 :: 2 :: months'
        val e = [(1,1,1)] = dates_in_months_challenge(dates', months'')
        val dates'' = (3,2,1) :: dates'
        val f = [(3,2,1), (1,1,1)] = dates_in_months_challenge(dates'', months'')
    in
        a andalso b andalso c andalso d andalso e andalso f
    end

fun reasonable_date(date : int*int*int) =
    let val year = get_year date
        val month = get_month date
        val day = get_day date
        val leap_year = (year mod 400 = 0) orelse (year mod 4 = 0 andalso
                                                   year mod 100 <> 0)
        val months = [31, if leap_year then 29 else 28, 31,
                      30, 31, 30, 31, 31, 30, 31, 30, 31]
        val month' = month mod 12
        val days_in_month = get_nth'(months, month')
        val appropriate_day = day > 0 andalso day <= days_in_month
    in
        year > 0 andalso month > 0 andalso month <= 12 andalso appropriate_day
    end

val test_reasonable_date =
    let val a = not(reasonable_date(0,0,0))
        val b = not(reasonable_date(0,1,1))
        val c = not(reasonable_date(1,0,1))
        val d = not(reasonable_date(1,1,0))
        val e = reasonable_date(2014, 10, 6)
        val f = reasonable_date(1991, 6, 19)
        val g = not(reasonable_date(1066, 13, 8))
        val h = not(reasonable_date(1492, 6, 36))
        val i = reasonable_date(1200, 2, 29)
        val j = not(reasonable_date(1700, 2, 29))
        val k = reasonable_date(2004, 2, 29)
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g andalso h
        andalso i andalso j andalso k
    end

val alL_tests_passing =
    test_is_older andalso test_number_in_month andalso test_dates_in_month andalso
    test_dates_in_months andalso test_get_nth andalso test_date_to_string andalso
    test_number_before_reaching_sum andalso test_what_month andalso test_month_range
    andalso test_oldest andalso test_number_in_months_challenge andalso
    test_dates_in_months_challenge andalso test_reasonable_date
