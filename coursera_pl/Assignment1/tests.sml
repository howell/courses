(*
Sam Caldwell
Assignment 1 Tests
Coursera Programming Languages 2014
*)

use "assignment1.sml";

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

val test_number_in_month =
    let val a = 0 = number_in_month([], 1)
        val b = 1 = number_in_month([(1,1,1)], 1)
        val c = 1 = number_in_month([(1,1,1), (1,2,1)], 1)
        val d = 2 = number_in_month([(1,1,1), (1,2,1), (3,1,45)], 1)
    in
        a andalso b andalso c andalso d
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

val test_get_nth =
    let val l = ["foo"]
        val a = "foo" = get_nth(l, 1)
        val l' = "bar" :: l
        val b = "bar" = get_nth(l', 1)
        val c = "foo" = get_nth(l', 2)
    in
        a andalso b andalso c
    end

val test_date_to_string =
    let val a = "January 20, 2013" = date_to_string(2013,1,20)
        val b = "July 16, 42" = date_to_string(42, 7, 16)
        val c = "October 31, 1492" = date_to_string(1492, 10, 31)
    in
        a andalso b andalso c
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
    
val test_month_range =
    let val a = [] = month_range(100, 50)
        val b = [1] = month_range(1, 1)
        val c = [1,2] = month_range(31, 32)
    in
        a andalso b andalso c
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

val test1 = is_older((1,2,3),(2,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3

val test9 = what_month(70) = 3

val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val all_tests_passing =
    test_is_older andalso test_number_in_month andalso test_dates_in_month andalso
    test_dates_in_months andalso test_get_nth andalso test_date_to_string andalso
    test_number_before_reaching_sum andalso test_what_month andalso test_month_range
    andalso test_oldest andalso test_number_in_months_challenge andalso
    test_dates_in_months_challenge andalso test_reasonable_date andalso test1 andalso
    test2 andalso test3 andalso test4 andalso test5 andalso test6 andalso test7 andalso
    test8 andalso test9 andalso test10 andalso test11
