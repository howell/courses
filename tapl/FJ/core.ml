open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   Class Table  ----------------------- *)

let this = TmVar("this")

type classtable = classdef list

let obj : classdef =
    { cname = "Object";
      super = "Object";
      cfields = [];
      ctor =  { ctorformals = []; supers = []; ctorfields = []; };
      methods = [];
    }

let isObj cls = cls.cname = "Object"

let emptyCT = [ obj ]

let addClass c ct = c :: ct

let addClasses cs ct = cs @ ct

let lookupCT name ct = List.find (fun cls -> cls.cname = name) ct

let rec foldUp fn init clsname ct =
    let rec walk acc cname =
        let cls = lookupCT cname ct in
        if isObj cls
        then fn acc cls
        else walk (fn acc cls) cls.super
    in
    walk init clsname

let searchUp fn clsname ct =
    let rec walk cname =
        let cls = lookupCT cname ct in
        if isObj cls
        then fn cls
        else match fn cls with
            Some(x) -> Some x
          | None    -> walk cls.super
    in
    walk clsname

let predUp p clsname ct =
    let r = searchUp (fun cls -> if p cls then Some(true) else None) clsname ct
    in
    match r with
        Some(_) -> true
      | None    -> false

let fields ct cname =
    foldUp (fun acc cls -> cls.cfields @ acc) [] cname ct

let formalType (_,t) = t
let formalName (n,_) = n
let fieldType (_,t) = t
let fieldName (n,_) = n

exception MethodNotFound of string * string

let findmethod mname cname ct =
    let search cls =
        try let mthd = List.find (fun mthd -> mthd.mname = mname) cls.methods in
            Some mthd
        with Not_found -> None
    in
    match searchUp search cname ct with
        Some mthd -> mthd
      | None      -> raise (MethodNotFound (mname, cname))

let mthdtype mthd =
    let fmls = List.map formalType mthd.mformals in
    TyArr(fmls, mthd.retTy)

let mtype mname cname ct =
    let mthd = findmethod mname cname ct in
    mthdtype mthd

let mbody mname cname ct =
    let mthd = findmethod mname cname ct in
    let fmls = List.map formalName mthd.mformals in
    (fmls, mthd.mbody)

exception IllegalOverrideType of ty

let override ct mname cname tarr =
    match tarr with
        TyArr(fmlCTys, resCTy) ->
            (match mtype mname cname ct with
                TyArr(fmlDTys, resDTy) ->
                    fmlCTys = fmlDTys && resCTy = resDTy
              | t -> raise (IllegalOverrideType t))
      | _ -> raise (IllegalOverrideType tarr)

(* ------------------------   SUBTYPING  ------------------------ *)

let rec subtype ct ctx tyS tyT =
    match (tyS, tyT) with
        (TyObj(sname), TyObj(tname)) ->
            predUp (fun cls -> cls.cname = tname) sname ct
      | _ -> false

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ct ctx t = match t with
    TmVar(x) -> getTypeFromContext ctx x
  | TmProj(t',fldname) ->
          (match typeof ct ctx t' with
            TyObj cname ->
                let flds = fields ct cname in
                try List.assoc fldname flds
                with Not_found -> error dummyinfo
                             ("Field "^fldname^" missing in "^cname)
          | _ -> error dummyinfo "Expected object type in field access")
  | TmInvk(trcv,mname,tms) ->
          let TyObj cname = typeof ct ctx trcv in
          let TyArr(fmlTs,resT) = mtype mname cname ct in
          let tmTs = List.map (typeof ct ctx) tms in
          (try if List.for_all2 (subtype ct ctx) tmTs fmlTs
              then resT
              else error dummyinfo "Parameter type mismatch"
          with _ -> error dummyinfo "Incorrect number of parameters")
  | TmNew(cname,fldTms) ->
          let fldTys = List.map fieldType (fields ct cname) in
          let tmTys = List.map (typeof ct ctx) fldTms in
          (try if List.for_all2 (subtype ct ctx) tmTys fldTys
              then TyObj cname
              else error dummyinfo "Constructor field type mismatch"
          with _ -> error dummyinfo "Incorrect number of parameters")
  | TmCast(tyT,tm) ->
          let tmTy = typeof ct ctx tm in
          if subtype ct ctx tmTy tyT
          then tyT      (* upcast *)
          else if subtype ct ctx tyT tmTy
            then tyT    (* downcast *)
            else tyT    (* stupid cast *)


let methodOk ct cls mthd =
    let emptyctx = emptycontext in
    let ctx = List.fold_left (fun acc (fname,tyF) ->
        addbinding acc fname (VarBind tyF)) emptyctx mthd.mformals in
    let ctx' = addbinding ctx "this" (VarBind (TyObj cls.cname)) in
    let tyBody = typeof ct ctx' mthd.mbody in
    let mtype = mthdtype mthd in
    let a = subtype ct emptyctx tyBody mthd.retTy in
    let b = override ct mthd.mname cls.cname mtype in
    a && b

let classOk ct cls =
    let supflds = fields ct cls.super in
    let ctor = cls.ctor in
    ctor.ctorformals = supflds @ cls.cfields &&
    ctor.supers = supflds &&
    ctor.ctorfields = cls.cfields &&
    List.for_all (methodOk ct cls) cls.methods

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isval ct ctx t =
    match t with
        TmNew(_,flds) -> List.for_all (isval ct ctx) flds
      | _             -> false


let indexof x xs =
    let rec search i xs = match xs with
        [] -> raise Not_found
      | x' :: xs' -> if x = x' then i else search (i+1) xs'
    in
    search 0 xs

let rec eval1 ct ctx t =
    let isval' = isval ct ctx in
    let allval = List.for_all isval' in
    match t with
  | TmProj(TmNew(cname,tms),fldname) when allval tms ->
        let fldNames = List.map fieldName (fields ct cname) in
        List.nth tms (indexof fldname fldNames)
  | TmProj(t1,fldname) ->
        let t1' = eval1 ct ctx t1 in
        TmProj(t1', fldname)
  | TmInvk(TmNew(cname,tms),mname,args) when allval tms && allval args ->
        let (fmlNames, bodyTm) = mbody mname cname ct in
        let r  = List.fold_left2 (fun tm fml v -> termSubst fml v tm)
                    bodyTm fmlNames args in
        termSubst "this" (TmNew(cname,tms)) r
  | TmInvk(TmNew(cname,tms),mname,args) when allval tms ->
        (match List.partition isval' args with
        (vs, tm1::tm1s) ->
            let tm1' = eval1 ct ctx tm1 in
            TmInvk(TmNew(cname,tms),mname,vs @ tm1'::tm1s)
      | _ -> error dummyinfo "Unexpected invoke form")
  | TmInvk(t1,mname,args) ->
        let t1' = eval1 ct ctx t1 in
        TmInvk(t1', mname, args)
  | TmNew(cname,tms) ->
        (match List.partition isval' tms with
        (vs, tm1::tm1s) ->
            let tm1' = eval1 ct ctx tm1 in
            TmNew(cname, vs @ tm1'::tm1s)
      | (_, []) -> raise NoRuleApplies)
  | TmCast(tyC,TmNew(cname,tms)) when allval tms ->
          if subtype ct ctx (TyObj cname) tyC
          then TmNew(cname, tms)
          else raise NoRuleApplies
  | TmCast(tyC,t1) ->
          let t1' = eval1 ct ctx t1 in
          TmCast(tyC, t1')
  | _ ->
          raise NoRuleApplies


let rec eval ct ctx t =
  try let t' = eval1 ct ctx t
      in eval ct ctx t'
  with NoRuleApplies -> t


