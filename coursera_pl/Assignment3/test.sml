(*
Sam Caldwell
Coursera PL
HW3 Tests
*)

use "assignment3.sml";

val test_only_capitals =
    let val a = ["A","B","C"] = only_capitals ["A","B","C"]
        val b = ["A","C"] = only_capitals ["A", "b", "C"]
        val c = [] = only_capitals []
    in
        a andalso b andalso c
    end

val test_longest_string1 =
    let val a = "bc" = longest_string1 ["A","bc","C"]
        val b = "" = longest_string1 []
        val c = "foo" = longest_string1 ["foo", "baz", "bar"]
        val d = longest_string1 ["a", "bc", "def"] = "def"
    in
        a andalso b andalso c andalso d
    end

val test_longest_string2 =
    let val a = "bc" = longest_string2 ["A", "bc", "C"]
        val b = "" = longest_string2 []
        val c = "bar" = longest_string2 ["foo", "baz", "bar"]
        val d = longest_string2 ["a", "bc", "def"] = "def"
    in
        a andalso b andalso c andalso d
    end

val test_longest_string3 =
    let val a = "bc" = longest_string3 ["A","bc","C"]
        val b = "" = longest_string3 []
        val c = "foo" = longest_string3 ["foo", "baz", "bar"]
        val d = longest_string3 ["a", "bc", "def"] = "def"
    in
        a andalso b andalso c andalso d
    end

val test_longest_string4 =
    let val a = "bc" = longest_string4 ["A", "bc", "C"]
        val b = "" = longest_string4 []
        val c = "bar" = longest_string4 ["foo", "baz", "bar"]
        val d = longest_string4 ["a", "bc", "def"] = "def"
    in
        a andalso b andalso c andalso d
    end

val test_longest_capitalized =
    let val a = longest_capitalized ["A","bc","C"] = "A"
        val b = longest_capitalized [] = ""
        val c = longest_capitalized ["A", "B", "CD"] = "CD"
    in
        a andalso b andalso c
    end

val test_rev_string =
    let val a = rev_string "abc" = "cba"
        val b = rev_string "" = ""
        val c = rev_string "A" = "A"
        val d = rev_string "fOo" = "oOf"
    in
        a andalso b andalso c andalso d
    end

val test_first_answer =
    let val a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
        val b = first_answer SOME [1,2,3] = 1
        val c = (first_answer SOME []) handle NoAnser => true
    in
        a andalso b andalso c
    end

val test_all_answers =
    let val a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
        val b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1] = SOME [1]
        val c = all_answers (fn x => SOME [x]) [] = SOME []
        val d = all_answers (fn x => if x = 1 then SOME [x,x] else NONE) [1,1,1] = SOME [1,1,1,1,1,1]
    in
        a andalso b andalso c andalso d
    end

val test_count_wildcards =
    let val a = count_wildcards Wildcard = 1
        val b = count_wildcards (Variable "hi") = 0
        val c = count_wildcards UnitP = 0
        val d = count_wildcards (TupleP []) = 0
        val e = count_wildcards (ConstructorP ("Foo", UnitP)) = 0
        val f = count_wildcards (TupleP [Wildcard, UnitP, Wildcard]) = 2
        val g = count_wildcards (ConstructorP ("Baz", TupleP [TupleP [Wildcard], Wildcard])) = 2
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g
    end

val test_count_wild_and_variable_lengths =
    let val a = count_wild_and_variable_lengths (Variable("a")) = 1
        val b = count_wild_and_variable_lengths (ConstructorP ("Foo", UnitP)) = 0
        val c = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "fooz"]) = 5
    in
        a andalso b andalso c
    end

val test_count_some_var =
    let val a = count_some_var ("x", Variable("x")) = 1
        val b = count_some_var ("x", Variable("xyz")) = 0
        val c = count_some_var ("foo", UnitP) = 0
        val d = count_some_var ("x", TupleP [Variable "x", Wildcard, Variable "x"]) = 2
    in
        a andalso b andalso c andalso d
    end

val test_check_pat =
    let val a = check_pat (Variable("x")) = true
        val b = check_pat (TupleP [Variable "x", Variable "y"]) = true
        val c = check_pat (TupleP [Variable "x", Variable "x"]) = false
    in
        a andalso b andalso c
    end

val test_match =
    let val a = match (Const(1), UnitP) = NONE
        val b = match (Const 1, ConstP 1) = SOME []
        val c = match (Const 1, ConstP 2) = NONE
        val d = match (Unit, UnitP) = SOME []
        val e = match (Const 10, Wildcard) = SOME []
        val f = match (Const 3, Variable "x") = SOME [("x", Const 3)]
        val g = match (Constructor ("x", Unit), ConstructorP ("x", Variable "y")) = SOME [("y", Unit)]
        val h = match (Constructor ("x", Unit), ConstructorP ("z", Variable "y")) = NONE
        val i = match (Tuple [Const 1], TupleP [Variable "x"]) = SOME [("x", Const 1)]
        val j = match (Tuple [Unit, Unit], TupleP [UnitP]) = NONE
        val k = match (Tuple [Unit, Const 1], TupleP [UnitP, UnitP]) = NONE
    in
        a andalso b andalso c andalso d andalso e andalso f andalso g andalso
        h andalso i andalso j andalso k
    end

val test_first_match =
    let val a = first_match Unit [UnitP] = SOME []
        val b = first_match Unit [] = NONE
        val c = first_match (Const 1) [UnitP] = NONE
        val d = first_match (Const 2) [Variable "x", Variable "y"] = SOME [("x", Const 2)]
    in
        a andalso b andalso c andalso d
    end
