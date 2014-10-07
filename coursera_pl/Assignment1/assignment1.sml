(*
Sam Caldwell
Assignment 1
Coursera Programming Languages 2014
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

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else let val month = hd months
             val months' = tl months
         in
             dates_in_month(dates, month) @ dates_in_months(dates, months')
         end

fun get_nth'(xs, n) =
    if n > 1
    then get_nth'(tl xs, n - 1)
    else hd xs

fun get_nth(xs : string list, n : int) =
    get_nth'(xs, n)

fun date_to_string(date : int*int*int) =
    let val dates = ["January", "February", "March", "April", "May", "June", "July",
                     "August", "September", "October", "November", "December"]
        val year = get_year date
        val month = get_month date
        val day   = get_day date
    in
        get_nth(dates, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
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

fun what_month(day : int) =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, months)
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

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, nub months)

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
