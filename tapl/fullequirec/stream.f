/* Exercise 20.1.2 */
Stream = Rec S. Unit -> {Nat, S};

hd = lambda s:Stream. (s unit).1;

tl = lambda s:Stream. (s unit).2;

plus = fix (lambda p:Nat->Nat->Nat.
lambda m:Nat. lambda n:Nat.
if iszero m then n else succ (p (pred m) n));

fibs = fix (lambda f:{Nat,Nat} -> Stream.
      lambda fs:{Nat,Nat}. lambda _:Unit.
        let fn = plus fs.1 fs.2 in
            {fn, f {fn, fs.1}}) {0, 1};

hd fibs;
hd (tl fibs);
hd (tl (tl fibs));
hd (tl (tl (tl fibs)));
hd (tl (tl (tl (tl fibs))));
hd (tl (tl (tl (tl (tl fibs)))));
hd (tl (tl (tl (tl (tl (tl fibs))))));
