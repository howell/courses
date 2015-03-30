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

/* Exercise 24.2.1 */

let {Stack, stk} = {*List Nat,
    { new     = nil [Nat],
      push    = cons [Nat],
      top     = head [Nat],
      pop     = tail [Nat],
      isempty = isnil [Nat] } }
as { Some Stack,
    { new     : Stack,
      push    : Nat -> Stack -> Stack,
      top     : Stack -> Nat,
      pop     : Stack -> Stack,
      isempty : Stack -> Bool } }
in
    stk.top (stk.push 8 (stk.push 42 (stk.push 5 stk.new)));

/* Exercise 24.2.2 */

let {Counter, cntr} = {*Ref Nat,
    { new = lambda _:Unit. ref 0,
      inc = lambda c:Ref Nat. c := succ (!c),
      get = lambda c:Ref Nat. !c  } }
as { Some Counter,
    { new : Unit -> Counter,
      inc : Counter -> Unit,
      get : Counter -> Nat  } }
in
    let c = cntr.new unit in
    (cntr.inc c; cntr.inc c; cntr.get c);

/* Exercise 24.2.3 */

Counter = { Some X, { state : X, methods : { get : X -> Nat, inc : X -> X }}};

counterADT = {*Nat,
              {state = 0,
               methods = {get = lambda x:Nat. x,
                          inc = lambda x:Nat. succ(x)}}}
             as Counter;

sendget =
lambda c:Counter.
    let {X, body} = c in
    body.methods.get body.state;

sendinc =
lambda c:Counter.
    let {X, body} = c in
    {*X,
     {state = body.methods.inc body.state,
      methods = body.methods }}
    as Counter;

let {X, body} = counterADT in
body.methods.get (body.methods.inc body.state);

sendget (sendinc counterADT);

iseven =
fix (lambda isev: Nat -> Bool.
    lambda n:Nat.
        if iszero n
        then true
        else if iszero (pred n)
             then false
             else isev (pred (pred n)));

FlipFlop = { Some X,
            { state : X,
              methods : { read : X -> Bool,
                          toggle : X -> X,
                          reset : X -> X }}};

f = let {X, body} = counterADT in
    {*Counter,
     {state = counterADT,
      methods =
      { read = lambda c:Counter. iseven (sendget c),
        toggle = sendinc,
        reset = lambda c:Counter. counterADT }}}
    as FlipFlop;

sendread =
lambda f:FlipFlop.
    let {X, body} = f in
    body.methods.read (body.state);

sendtoggle =
lambda f:FlipFlop.
    let {X, body} = f in
    {*X,
     {state = body.methods.toggle body.state,
      methods = body.methods }}
    as FlipFlop;

sendread (sendtoggle (sendtoggle (sendtoggle f)));

/* Exercise 24.2.4 */

MCounter = { Some X,
            { state : X,
              methods : { get : X -> Nat,
                          inc : X -> Unit }}};
mcounter = {*Ref Nat,
            {state = ref 0,
             methods =
             {get = lambda x:Ref Nat. !x,
              inc = lambda x:Ref Nat. x := succ (!x) }}}
           as MCounter;

sendmget =
lambda c:MCounter.
    let {X, body} = c in
    body.methods.get body.state;

sendminc =
lambda c:MCounter.
    let {X, body} = c in
    body.methods.inc body.state;

sendminc mcounter; sendminc mcounter; sendminc mcounter; sendmget mcounter;
