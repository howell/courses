/* Exercise 20.1.5 */
D = Rec X. <nat:Nat, fn:X->X, rcd:Nat->X>;

fix_D =
lambda f:D->D.
  (lambda x:(Rec A.A->D). f (x x))
  (lambda x:(Rec A.A->D). f (x x));

diverge_D = lambda _:Unit. fix_D (lambda x:D. x);

lam = lambda f:D->D. <fn=f> as D;

ap = lambda f:D. lambda a:D.
    case f of
        <nat=n> ==> diverge_D unit
      | <fn=f>  ==> f a
      | <rcd=r> ==> diverge_D unit;

suc = lambda f:D. case f of
                    <nat=n> ==> <nat=succ n> as D
                  | <fn=f>  ==> diverge_D unit
                  | <rcd=r> ==> diverge_D unit;

zro = <nat=0> as D;

proj = lambda f:D. lambda l:Nat.
    case f of
        <nat=n> ==> diverge_D unit
      | <fn=f>  ==> diverge_D unit
      | <rcd=r> ==> r l;

equals = fix (lambda eq:Nat->Nat->Bool.
    lambda m:Nat. lambda n:Nat.
        if iszero m
        then iszero n
        else if iszero n
             then false
             else eq (pred m) (pred n));

emptyrcd = lambda _:Nat. diverge_D unit;

extendrcd = lambda rcd:Nat->D. lambda l:Nat. lambda d:D.
    lambda k:Nat.
        if equals k l
        then d
        else rcd k;

rcd = lambda r:Nat->D. <rcd=r> as D;

proj (rcd (extendrcd emptyrcd 0 (suc zro))) 0;
