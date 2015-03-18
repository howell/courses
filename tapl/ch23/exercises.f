/* Examples for testing */

Pair = lambda X. lambda Y. All R. (X->Y->R) -> R;

pair = lambda X.lambda Y.lambda x:X.lambda y:Y.lambda R.lambda p:X->Y->R.p x y;

f = lambda X.lambda Y.lambda f:Pair X Y. f;

fst = lambda X.lambda Y.lambda p:Pair X Y.p [X] (lambda x:X.lambda y:Y.x);
snd = lambda X.lambda Y.lambda p:Pair X Y.p [Y] (lambda x:X.lambda y:Y.y);

pr = pair [Nat] [Bool] 0 false;
fst [Nat] [Bool] pr;
snd [Nat] [Bool] pr;

List = lambda X. All R. (X->R->R) -> R -> R;

diverge =
lambda X.
  lambda _:Unit.
  fix (lambda x:X. x);

nil = lambda X.
      (lambda R. lambda c:X->R->R. lambda n:R. n)
      as List X;

cons =
lambda X.
  lambda hd:X. lambda tl: List X.
     (lambda R. lambda c:X->R->R. lambda n:R. c hd (tl [R] c n))
     as List X;

isnil =
lambda X.
  lambda l: List X.
    l [Bool] (lambda hd:X. lambda tl:Bool. false) true;

head =
lambda X.
  lambda l: List X.
    (l [Unit->X] (lambda hd:X. lambda tl:Unit->X. lambda _:Unit. hd) (diverge [X]))
    unit;

tail =
lambda X.
  lambda l: List X.
    (fst [List X] [List X] (
      l [Pair (List X) (List X)]
        (lambda hd: X. lambda tl: Pair (List X) (List X).
          pair [List X] [List X]
            (snd [List X] [List X] tl)
            (cons [X] hd (snd [List X] [List X] tl)))
        (pair [List X] [List X] (nil [X]) (nil [X]))))
    as List X;

/* Exercise 23.4.3 */
append =
  lambda X.
    lambda l1: List X.
      lambda l2: List X.
        l1 [List X] (cons [X]) l2;

reverse =
  lambda X.
    lambda xs: List X.
      xs [List X] (lambda x:X. lambda acc: List X.
                    append [X] acc (cons [X] x (nil [X])))
                  (nil [X]);

ls = cons [Nat] 4
       (cons [Nat] 2
         (cons [Nat] 3
           (cons [Nat] 0
             (nil [Nat]))));

/* Exercise 23.4.5 */

CBool X = X -> X -> X;

and = lambda X.
        lambda a:CBool X.
          lambda b:CBool X.
              (lambda t:X.
                lambda f:X.
                  a (b t f) f) as CBool X;

/* Exercise 23.4.11 */

head' =
lambda X.
  lambda l: List X.
    lambda err: X.
    (l [X] (lambda hd:X. lambda tl:X. hd) err);

/* Exercise 23.4.12 */
insert =
  lambda X.
    lambda lte: X -> X -> Bool.
      lambda xs: List X.
        lambda x: X.
/*
 * If we found x, then just return the accumulated list. Otherwise, x must be
 * smaller than all the elements in the list, so add it to the front.
 */
          (lambda pr: Pair Bool (List X).
            if fst [Bool] [List X] pr
            then snd [Bool] [List X] pr
            else cons [X] x (snd [Bool] [List X] pr))
/*
 * In the accumulator, keep a flag marking whether we've inserted x into the
 * list as the first element, and the list so far (possibly including x) in the
 * second element
 */
          (xs [Pair Bool (List X)]
            (lambda y: X.
              lambda acc: Pair Bool (List X).
                if fst [Bool] [List X] acc
                then pair [Bool] [List X]
                       true (cons [X] y (snd [Bool] [List X] acc))
                else if lte y x
                     then pair [Bool] [List X]
                            true (cons [X] y
                                   (cons [X] x
                                     (snd [Bool] [List X] acc)))
                     else pair [Bool] [List X]
                            false (cons [X] y (snd [Bool] [List X] acc)))
            (pair [Bool] [List X] false (nil [X])));

natlte =
  fix (lambda lte: Nat -> Nat -> Bool.
    lambda x: Nat.
      lambda y: Nat.
        if iszero x
        then true
        else if iszero y
             then false
             else lte (pred x) (pred y));

sort =
  lambda X.
    lambda lte: X -> X -> Bool.
      lambda xs: List X.
        xs [List X] (lambda x: X. lambda acc: List X.
                      insert [X] lte acc x)
                    (nil [X]);

sort [Nat] natlte ls;
