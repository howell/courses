/* Exercise 20.1.1 */
NatTree = Rec NT. <leaf:Unit, branch: { l:NT, n:Nat, r:NT }>;

empty = <leaf = unit> as NatTree;

lte = fix (lambda f:Nat->Nat->Bool.
     lambda m:Nat. lambda n:Nat.
        if iszero m
        then true
        else (if iszero n
             then false
             else f (pred m) (pred n)));

insert = fix (lambda ins:Nat->NatTree->NatTree.
    lambda n:Nat. lambda nt:NatTree.
        case nt of
        <leaf=u> ==> <branch={ l=empty, n=n, r=empty }> as NatTree
        | <branch=b> ==>
             (if lte n b.n
              then <branch={ l=ins n b.l, n=b.n, r=b.r }> as NatTree
              else <branch={ l=b.l, n=b.n, r=ins n b.r }> as NatTree));

NatList = Rec X. <nil:Unit, cons:{Nat,X}>;

nil = <nil=unit> as NatList;

cons = lambda n:Nat. lambda l:NatList. <cons={n,l}> as NatList;

isnil = lambda l:NatList.
case l of
<nil=u> ==> true
| <cons=p> ==> false;

hd = lambda l:NatList.
case l of
<nil=u> ==> 0
| <cons=p> ==> p.1;

tl = lambda l:NatList.
case l of
<nil=u> ==> l
| <cons=p> ==> p.2;

append = fix (lambda apnd:NatList -> NatList -> NatList.
    lambda xs:NatList. lambda ys:NatList.
        if isnil xs
        then ys
        else cons (hd xs) (apnd (tl xs) ys));

labels = fix (lambda lbls:NatTree -> NatList.
    lambda nt:NatTree.
        case nt of
            <leaf=u> ==> nil
          | <branch=b> ==> (append (lbls b.l) (cons b.n (lbls b.r))));

labels (insert 5 (insert 4 (insert 1 (insert 2 (insert 3 empty)))));
