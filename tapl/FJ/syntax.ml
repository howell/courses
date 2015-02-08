open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

(*
type ty =
    TyVar of int * int
  | TyId of string
  | TyTop
  | TyArr of ty * ty
  | TyBool
  | TyRecord of (string * ty) list
  | TyString
  | TyUnit
  | TyFloat
  | TyNat
*)

(*
type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmLet of info * string * term * term
  | TmFix of info * term
  | TmString of info * string
  | TmUnit of info
  | TmAscribe of info * term * ty
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
*)

type ty =
    TyObj of string
  | TyArr of ty list * ty

type term =
    TmVar of string
  | TmProj of term * string
  | TmInvk of term * string * term list
  | TmNew of string * term list
  | TmCast of ty * term

type formal = string * ty
type field  = string * ty

type constructor =
    { ctorformals : formal list;
      supers      : field list;
      ctorfields  : field list;
    }

type methd =
    { mname    : string;
      retTy    : ty;
      mformals : formal list;
      mbody    : term;
    }

type classdef =
    { cname    : string;
      super    : string;
      cfields  : field list;
      ctor     : constructor;
      methods  : methd list;
    }

type binding =
    NameBind 
  | TyVarBind
  | VarBind of ty
  | TmAbbBind of term * (ty option)
  | TyAbbBind of ty

type context = (string * binding) list

type command =
  | Eval of info * term
  | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

(*
let tymap onvar c tyT = 
  let rec walk c tyT = match tyT with
    TyVar(x,n) -> onvar c x n
  | TyId(b) as tyT -> tyT
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyTop -> TyTop
  | TyBool -> TyBool
  | TyRecord(fieldtys) -> TyRecord(List.map (fun (li,tyTi) -> (li, walk c tyTi)) fieldtys)
  | TyString -> TyString
  | TyUnit -> TyUnit
  | TyFloat -> TyFloat
  | TyNat -> TyNat
  in walk c tyT

let tmmap onvar ontype c t = 
  let rec walk c t = match t with
    TmInert(fi,tyT) -> TmInert(fi,ontype c tyT)
  | TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,tyT1,t2) -> TmAbs(fi,x,ontype c tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmProj(fi,t1,l) -> TmProj(fi,walk c t1,l)
  | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->
                                               (li,walk c ti))
                                    fields)
  | TmLet(fi,x,t1,t2) -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  | TmFix(fi,t1) -> TmFix(fi,walk c t1)
  | TmString _ as t -> t
  | TmUnit(fi) as t -> t
  | TmAscribe(fi,t1,tyT1) -> TmAscribe(fi,walk c t1,ontype c tyT1)
  | TmFloat _ as t -> t
  | TmTimesfloat(fi,t1,t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
  | TmZero(fi)      -> TmZero(fi)
  | TmSucc(fi,t1)   -> TmSucc(fi, walk c t1)
  | TmPred(fi,t1)   -> TmPred(fi, walk c t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi, walk c t1)
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d))
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) 
                     else TmVar(fi,x,n+d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | TyVarBind -> TyVarBind
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | TmAbbBind(t,tyT_opt) ->
     let tyT_opt' = match tyT_opt with
                      None->None
                    | Some(tyT) -> Some(typeShift d tyT) in
     TmAbbBind(termShift d t, tyT_opt')
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)
*)

(* ---------------------------------------------------------------------- *)
(* Substitution *)

(* x = symbol (string) to substitue;
 * v = x's replacement (should be a value)
 * tm = the term to search in
 *)
let rec termSubst x v tm =
    match tm with
    TmVar n -> if n = x then v else tm
  | TmProj(tm',f) -> TmProj(termSubst x v tm', f)
  | TmInvk(tm',m,args) -> let args' = List.map (termSubst x v) args in
                             TmInvk(termSubst x v tm', m, args')
  | TmNew(tyT, args) -> TmNew(tyT, List.map (termSubst x v) args)
  | TmCast(tyT, tm') -> TmCast(tyT, termSubst x v tm')

(*
let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun j tyT -> tyT)
    j t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
*)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx name =
  try
    List.assoc name ctx
  with Not_found ->
    let msg =
      Printf.sprintf "Variable lookup failure: %s, ctx size: %d" in
    error fi (msg name (List.length ctx))
 let getTypeFromContext ctx name =
   match getbinding dummyinfo ctx name with
         VarBind(tyT) -> tyT
     | TmAbbBind(_,Some(tyT)) -> tyT
     | TmAbbBind(_,None) -> error dummyinfo ("No type recorded for variable "
                                        ^ name)
     | _ -> error dummyinfo 
       ("getTypeFromContext: Wrong kind of binding for variable " 
         ^ name) 

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

(*
let tmInfo t = match t with
    TmInert(fi,_) -> fi
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmProj(fi,_,_) -> fi
  | TmRecord(fi,_) -> fi
  | TmLet(fi,_,_,_) -> fi
  | TmFix(fi,_) -> fi
  | TmString(fi,_) -> fi
  | TmUnit(fi) -> fi
  | TmAscribe(fi,_,_) -> fi
  | TmFloat(fi,_) -> fi
  | TmTimesfloat(fi,_,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi 
*)

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_) -> true
  | _ -> false

let rec printTys ctx tys =
    let pf tyi = printty_Type false ctx tyi in
    match tys with
        [] -> ()
      | [tyi] -> pf tyi
      | tyi::rest ->
            pf tyi; pr" -> ";
            printTys ctx rest

and printty_Type outer ctx tyT =
    printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx  tyT = match tyT with 
    TyArr(tyTs,tyT2) ->
      obox0(); 
      printTys ctx tyTs;
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | _ -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyObj(n) -> pr n
  | _ -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT

let rec printTms ctx tms =
    let pf ti = printtm_Term false ctx ti in
    match tms with
        [] -> ()
      | [ti] -> pf ti
      | ti::rest ->
            pf ti; pr","; print_space();
            printTms ctx rest

and printtm_Term outer ctx t = match t with
    t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmCast(tyT,tm) ->
      obox0();
      pr "("; printty ctx tyT; pr ")";
      print_space();
      printtm_ATerm false ctx tm;
      cbox()
  | t -> printtm_PathTerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr l
  | TmInvk(t1, l, tms) ->
      printtm_ATerm false ctx t1; pr "."; pr l;
      pr "("; open_hovbox 0; printTms ctx tms; pr ")"; cbox()
  | TmNew(cname, tms) ->
      pr "new "; pr cname;
      pr "("; open_hovbox 0; printTms ctx tms; pr ")"; cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
  | TmVar(n) ->
      pr n
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TmAbbBind(t,tyT) -> pr "= "; printtm ctx t
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT 


