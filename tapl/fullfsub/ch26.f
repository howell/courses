SNat = All X <: Top. All S <: X. All Z <: X. (X -> S) -> Z -> X;

SZero = All X <: Top. All S <: X. All Z <: X. (X -> S) -> Z -> Z;

SPos = All X <: Top. All S <: X. All Z <: X. (X -> S) -> Z -> S;

szero =
(lambda X. lambda S <: X. lambda Z <: X.
    lambda s: X -> S. lambda z: Z.
        z) as SZero;

spluspp =
lambda n: SPos. lambda m: SPos.
    (lambda X <: Top. lambda S <: X. lambda Z <: X.
        lambda s: X -> S. lambda z: Z.
            n [X] [S] [S] s (m [X] [S] [Z] s z)) as SPos;

/* Exercise 26.3.4 */

spluszz =
lambda n: SZero. lambda m: SZero.
    (lambda X <: Top. lambda S <: X. lambda Z <: X.
        lambda s: X -> S. lambda z: Z.
            n [X] [S] [Z] s (m [X] [S] [Z] s z)) as SZero;

spluspn =
lambda n: SPos. lambda m: SNat.
    (lambda X <: Top. lambda S <: X. lambda Z <: X.
        lambda s: X -> S. lambda z: Z.
            m [S] [S] [S] s (n [X] [S] [Z] s z)) as SPos;

/* Exercise 26.3.5 */

SBool = All X. All T <: X. All F <: X. T -> F -> X;

STrue = All X. All T <: X. All F <: X. T -> F -> T;

SFalse = All X. All T <: X. All F <: X. T -> F -> F;

strue =
(lambda X. lambda T <: X. lambda F <: X.
    lambda t: T. lambda f: F.
        t) as STrue;

sfalse =
(lambda X. lambda T <: X. lambda F <: X.
    lambda t: T. lambda f: F.
        f) as SFalse;

notft =
lambda x: SFalse.
    (lambda X. lambda T <: X. lambda F <: X.
        lambda t: T. lambda f: F.
            x [X] [F] [T] f t) as STrue;

nottf =
lambda x: STrue.
    (lambda X. lambda T <: X. lambda F <: X.
        lambda t: T. lambda f: F.
            x [X] [F] [T] f t) as SFalse;

/* Exercise 26.5.3 */

Counters = { Some Counter,
            { methods : { new : Counter,
                          get : Counter -> Nat,
                          inc : Counter -> Counter },
              resetCounter : { Some ResetCounter <: Counter,
                              { methods : { new : ResetCounter,
                                            get : ResetCounter -> Nat,
                                            inc : ResetCounter -> ResetCounter,
                                            reset : ResetCounter -> ResetCounter }}}}};

counters = { *Nat,
            { methods = { new = 1,
                          get = lambda c:Nat. c,
                          inc = lambda c:Nat. succ c },
              resetCounter = { *Nat,
                              { methods = { new = 1,
                                            get = lambda c:Nat. c,
                                            inc = lambda c:Nat. succ c,
                                            reset = lambda _:Nat. 1 }}}
                             as { Some ResetCounter <: Nat,
                                 { methods : { new : ResetCounter,
                                               get : ResetCounter -> Nat,
                                               inc : ResetCounter -> ResetCounter,
                                               reset : ResetCounter -> ResetCounter }}}}}
           as Counters;

let {Counter, cntr} = counters in
    let {ResetCounter, rcntr} = cntr.resetCounter in
        cntr.methods.get (rcntr.methods.reset (rcntr.methods.inc rcntr.methods.new));
