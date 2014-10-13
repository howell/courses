(*
Sam Caldwell
Coursera PL
HW2
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun elem(x, xs) =
    case xs of
        [] => false
     | x' :: xs' => x = x' orelse elem(x, xs')

fun all_except_option(s : string, xs : string list) =
    let fun consOption(x, opts) =
        case opts of
            NONE     => NONE
	  | SOME xs  => SOME (x :: xs)
    in
        case xs of
            []       => NONE
          | x :: xs' => if same_string(x, s)
                        then SOME xs'
                        else consOption(x, all_except_option(s, xs'))
    end

fun get_substitutions1(subs : string list list, s : string) =
    case subs of
        []           => []
      | sub :: subs' => case all_except_option(s, sub) of
                            NONE     => get_substitutions1(subs', s)
                          | SOME xs  => xs @ get_substitutions1(subs', s)

fun get_substitutions2(subs : string list list, s : string) =
    let fun go(subs, acc) =
        case subs of
            []           => acc
	  | sub :: subs' => case all_except_option(s, sub) of
                               NONE    => go (subs', acc)
                             | SOME xs => go (subs', acc @ xs)
    in
        go (subs, [])
    end

type full_name = {first : string, middle: string, last: string}

fun similar_names(subs : string list list, name : full_name) =
    let val {first = fst, middle = mid, last = lst } = name
        fun add_names(alts) =
            case alts of
                [] => []
              | x :: alts' =>  {first = x, middle = mid, last = lst} :: add_names(alts')
    in
        name :: add_names(get_substitutions2(subs, fst))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun suit_color Clubs    = Black
  | suit_color Spades   = Black
  | suit_color Diamonds = Red
  | suit_color Hearts   = Red

fun card_color c =
    case c of
        (s, _) => suit_color s

fun rank_value (Num n) = n
  | rank_value Ace     = 11
  | rank_value _       = 10

fun card_value c =
    case c of
        (_, r) => rank_value r

fun remove_card (cs : card list, c : card, e : exn) =
    case cs of
        [] => raise e
      | c' :: cs' => if c = c' then cs' else c' :: remove_card (cs', c, e)

fun all_same_color cs =
    let fun all_color (c, cds) =
        case cds of
            [] => true
          | cd :: cds => card_color cd = c andalso all_color (c, cds)
    in
        case cs of
            [] => true
          | cd :: cds => all_color (card_color cd, cds)
    end

fun sum_cards cs =
    let fun sum_cards' (cs, acc) =
        case cs of
            [] => acc
          | c :: cs' => sum_cards' (cs', card_value c + acc)
    in
        sum_cards' (cs, 0)
    end

fun score (cds, goal) =
    let val sum_cds = sum_cards cds
        val prelim  = if sum_cds > goal
                      then 3 * (sum_cds - goal)
                      else goal - sum_cds
    in
        if all_same_color cds
        then prelim div 2
        else prelim
    end

fun officiate (cds, moves, goal) =
    let fun run_move (card_list, held_cards, mv, moves, run) =
        case mv of
            Discard c => run(card_list, remove_card(held_cards, c, IllegalMove), moves)
          | Draw => case card_list of
                        [] => score (held_cards, goal)
                      | c :: cl' => let val hld = c :: held_cards
                                        val sc = score(hld, goal)
                                    in
                                        if sc > goal
                                        then sc
                                        else run(cl', hld, moves)
                                    end
        fun run (card_list, held_cards, moves) =
        case moves of
            [] => score (held_cards, goal)
          | m :: moves' => run_move (card_list, held_cards, m, moves', run)
    in
        run (cds, [], moves)
    end
