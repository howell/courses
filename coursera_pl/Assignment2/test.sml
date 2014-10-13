(*
Sam Caldwell
Coursera PL
Assignment 2 Tests
*)

use "assignment2.sml";

val test_all_except_option =
    let val a = SOME [] = all_except_option("string", ["string"])
        val b = NONE = all_except_option("string", [])
        val c = NONE = all_except_option("foo", ["bar", "baz", "quux"])
        val d = SOME ["bar", "baz", "quux"] = all_except_option("foo", ["foo", "bar", "baz", "quux"])
        val e = SOME ["bar", "baz", "quux"] = all_except_option("foo", ["bar", "foo", "baz", "quux"])
        val f = SOME ["bar", "baz", "quux"] = all_except_option("foo", ["bar", "baz", "foo", "quux"])
        val g = SOME ["bar", "baz", "quux"] = all_except_option("foo", ["bar", "baz", "quux", "foo"])
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g
    end

val test_get_substitutions1 =
    let val a = [] = get_substitutions1([["foo"],["there"]], "foo")
        val b = ["Fredrick","Freddie","F"] = get_substitutions1([["Fred","Fredrick"],
                                             ["Elizabeth","Betty"],
                                             ["Freddie","Fred","F"]], "Fred")
        val c = ["Jeffrey","Geoff","Jeffrey"] = get_substitutions1([["Fred","Fredrick"]
                                                ,["Jeff","Jeffrey"],
                                                ["Geoff","Jeff","Jeffrey"]], "Jeff")
    in
        a andalso b andalso c
    end
         
val test_get_substitutions2 =
    let val a = [] = get_substitutions2([["foo"],["there"]], "foo")
        val b = ["Fredrick","Freddie","F"] = get_substitutions2([["Fred","Fredrick"],
                                             ["Elizabeth","Betty"],
                                             ["Freddie","Fred","F"]], "Fred")
        val c = ["Jeffrey","Geoff","Jeffrey"] = get_substitutions2([["Fred","Fredrick"]
                                                ,["Jeff","Jeffrey"],
                                                ["Geoff","Jeff","Jeffrey"]], "Jeff")
    in
        a andalso b andalso c
    end
         
val test_similar_names =
    let val a = [{first="Fred", last="Smith", middle="W"},
                {first="Fredrick", last="Smith", middle="W"},
                {first="Freddie", last="Smith", middle="W"},
                {first="F", last="Smith", middle="W"}] =
                similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],
                               ["Freddie","Fred","F"]],
                              {first="Fred", middle="W", last="Smith"})
    in
        a
    end

val test_card_color =
    let val a = Black = card_color (Clubs, Num 2)
        val b = Black = card_color (Clubs, Queen)
        val c = Red = card_color (Diamonds, Num 9)
        val d = Red = card_color (Diamonds, Jack)
        val e = Black = card_color (Spades, Num 7)
        val f = Black = card_color (Spades, Ace)
        val g = Red = card_color (Hearts, Num 10)
        val h = Red = card_color (Hearts, King)
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g andalso h
    end

val test_card_value =
    let val a = 2 = card_value((Clubs, Num 2))
        val b = 11 = card_value (Spades, Ace)
        val c = 10 = card_value (Hearts, Num 10)
        val d = 10 = card_value (Diamonds, Jack)
    in
        a andalso b andalso c andalso d
    end

val ha = (Hearts, Ace)
val sa = (Spades, Ace)
val c10 = (Clubs, Num 10)
val dj = (Diamonds, Jack)
val h4 = (Hearts, Num 4)

val test_remove_card =
    let val a = [] = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove)
        val b = [(Hearts, Num 999)] = (remove_card([], (Spades, Ace), IllegalMove)
                                       handle IllegalMove => [(Hearts, Num 999)])
        val c = [(Clubs, Num 4)] = remove_card([(Clubs, Num 4), (Clubs, Num 4)],
                                   (Clubs, Num 4), IllegalMove)
        val d = [ha, sa] = remove_card([ha, c10, sa], c10, IllegalMove)
    in
        a andalso b andalso c andalso d
    end

val test_all_same_color =
    let val a = all_same_color([(Hearts, Ace), (Hearts, Ace)])
        val b = all_same_color([h4, dj])
        val c = all_same_color([dj, h4])
        val d = all_same_color([])
        val e = all_same_color([sa, c10])
        val f = all_same_color([ha, dj, h4])
        val g = not (all_same_color([ha, sa]))
        val h = not (all_same_color ([ha, h4, c10]))
        val i = not (all_same_color ([c10, sa, dj]))
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g andalso h
        andalso i
    end

val test_sum_cards =
    let val a = 4 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)])
        val b = 21 = sum_cards([ha, c10])
        val c = 0 = sum_cards([])
        val d = 14 = sum_cards([dj, h4])
    in
        a andalso b andalso c andalso d
    end

val test_score =
    let val a = 4 = score([(Hearts, Num 2),(Clubs, Num 4)],10)
        val b = 0 = score([dj], 10)
        val c = 16 = score([sa, c10], 10)
    in
        a andalso b andalso c
    end

val test_officiate =
    let val a = 6 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15)
        val b = 3 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                              [Draw,Draw,Draw,Draw,Draw],
                              42)
        val c = (9999999 = officiate([(Clubs,Jack),(Spades,Num(8))],
                                     [Draw,Discard(Hearts,Jack)],
                                     42))
                handle IllegalMove => true
    in
        a andalso b andalso c
    end
