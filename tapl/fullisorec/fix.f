/* Exercise 20.2.1 */
T = Nat;

Mu = Rec A. A -> T;
MuBody = Mu -> T;

fix_T = lambda f:T->T.
  (lambda x:Mu. f ((unfold [Mu] x) x))
  (fold [Mu] (lambda x:Mu. f ((unfold [Mu] x) x)));

diverge_t = lambda _:Unit. fix_T (lambda x:T. x);

MuTT = Rec A. A -> T -> T -> T;
MuttBody = MuTT -> T -> T -> T;

fix_TTT = lambda f: (T->T->T)->(T->T->T).
  (lambda x:MuTT. f ((unfold [MuTT] x) x))
  (fold [MuTT] (lambda x:MuTT. f ((unfold [MuTT] x) x)));

D = Rec X. X -> X;
DBody = D -> D;

lam = lambda f:D->D. fold [D] f;

ap = lambda f:D. lambda a:D. (unfold [D] f) a;

fixD = lam (lambda f:D.
        ap (lam (lambda x:D. ap f (ap x x)))
           (lam (lambda x:D. ap f (ap x x))));

Hungry = Rec A. Nat -> A;
HungryBody = Nat -> Hungry;

hungy = fix (lambda f:Nat->Hungry. lambda n:Nat. fold [Hungry] f);
