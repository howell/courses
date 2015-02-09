/* Exercise 20.1.4 */

D = Rec X. <nat:Nat, bool:Bool, fn:X->X>;

fix_D =
lambda f:D->D.
  (lambda x:(Rec A.A->D). f (x x))
  (lambda x:(Rec A.A->D). f (x x));

diverge_D = lambda _:Unit. fix_D (lambda x:D. x);

lam = lambda f:D->D. <fn=f> as D;

ap = lambda f:D. lambda a:D.
    case f of
        <nat=n>   ==> diverge_D unit
      | <bool=b>  ==> diverge_D unit
      | <fn=f>    ==> f a;

suc = lambda f:D. case f of
                    <nat=n>   ==> <nat=succ n> as D
                  | <bool=b>  ==> diverge_D unit
                  | <fn=f>    ==> diverge_D unit;

zro = <nat=0> as D;

tru = <bool=true> as D;

fls = <bool=false> as D;

ifthenelse = lambda cond:D. lambda tbr:Unit->D. lambda fbr:Unit->D.
    case cond of
        <nat=n>   ==> diverge_D unit
      | <bool=b>  ==> (if b then tbr unit else fbr unit)
      | <fn=f>    ==> diverge_D unit;

ex1 = ifthenelse fls
        (lambda _:Unit. suc zro)
        (lambda _:Unit. zro);

ex2 = ifthenelse fls
        (lambda _:Unit. suc zro)
        (lambda _:Unit. fls);

/* evaluates to <nat=0> */
ex1;
/* evaluates to <bool=false> */
ex2;
