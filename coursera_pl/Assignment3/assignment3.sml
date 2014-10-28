(*
Sam Caldwell
Coursera PL
HW3
*)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* Flip the order a function takes its arguments *)
fun flip f a b = f b a

fun curry f a b = f (a,b)

fun uncurry f (a,b) = f a b

val only_capitals = List.filter (Char.isUpper o flip (curry String.sub) 0)

(* favor the second argument *)
fun longer_string a b = if String.size a > String.size b then a else b

val longest_string1 = List.foldl (uncurry longer_string) ""

val longest_string2' = longest_string1 o List.rev

val longest_string2 = List.foldl (uncurry (flip longer_string)) ""

fun longest_string_helper p =
    let fun helper (s,acc) = if p (String.size s, String.size acc) then s else acc
    in
        List.foldl helper ""
    end

fun gt a b = a > b
fun gte a b = a >= b

val longest_string3 = longest_string_helper (uncurry gt)

val longest_string4 = longest_string_helper (uncurry gte)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer p xs =
    case xs of
        []        => raise NoAnswer
      | x :: xs'  => (case p x of
                          NONE   => first_answer p xs'
                        | SOME a => a)

fun all_answers p =
    let fun go acc xs =
        case xs of
            []        => SOME acc
          | x :: xs'  => case p x of
                             NONE     => NONE
                           | SOME bs  => go (acc @ bs) xs'
    in
        go []
    end

fun const a b = a

val count_wildcards = g (const 1) (const 0)

val count_wild_and_variable_lengths = g (const 1) String.size

