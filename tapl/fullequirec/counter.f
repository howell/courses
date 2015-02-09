/* Exercise 20.1.3 */
Counter = Rec C. {get: Nat,
                  inc: Unit -> C,
                  dec: Unit -> C,
                  backup: Unit -> C,
                  reset: Unit -> C };

c =
let create =
  fix
    (lambda cr: {x:Nat, b:Nat} -> Counter.
      lambda s: {x:Nat, b:Nat}.
        {get = s.x,
         inc = lambda _:Unit. cr {x=succ(s.x), b=s.b},
         dec = lambda _:Unit. cr {x=pred(s.x), b=s.b},
         backup = lambda _:Unit. cr {x=s.x, b=s.x},
         reset = lambda _:Unit. cr {x=s.b, b=s.b}
        })
in
  create {x=0, b=0};

c1 = c.inc unit;
c2 = c1.inc unit;
c2.get;
c3 = c2.backup unit;
c4 = c3.inc unit;
c5 = c4.inc unit;
c6 = c5.inc unit;
c6.get;
(c6.reset unit).get;
