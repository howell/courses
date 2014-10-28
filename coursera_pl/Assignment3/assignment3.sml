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

fun const a b = a

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

val count_wildcards = g (const 1) (const 0)

val count_wild_and_variable_lengths = g (const 1) String.size

fun count_some_var (var, pat) = g (const 0) (fn n => if n = var then 1 else 0) pat

fun eq a b = a = b

val check_pat =
    let fun vars pat =
            case pat of
	        Variable v          => [v]
              | ConstructorP (_, p) => vars p
              | TupleP ps           => List.concat (List.map vars ps)
              | _                   => []
        fun uniq xs =
            case xs of
                []       => true
              | x :: xs' => not (List.exists (eq x) xs') andalso uniq xs'
    in
        uniq o vars
    end

fun match (_, Wildcard)                            = SOME []
  | match (Unit, UnitP)                            = SOME []
  | match (v, Variable s)                          = SOME [(s, v)]
  | match (Const n, ConstP m)                      = if n = m then SOME [] else NONE
  | match (Constructor (s,v), ConstructorP (t, p)) = if s = t then match (v,p) else NONE
  | match (Tuple vs, TupleP ps)                    =
        ((all_answers match (ListPair.zipEq (vs, ps))) handle ListPair.UnequalLengths => NONE)
  | match _                                        = NONE

fun first_match v pats = SOME (first_answer (curry match v) pats) handle NoAnswer => NONE

