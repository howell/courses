open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmAbs(_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | _ -> false

exception NoRuleApplies

let rec eval1 ctx t = match t with
    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   SUBTYPING  ------------------------ *)

let rec subtype tyS tyT =
   (=) tyS tyT ||
   match (tyS,tyT) with
     (_,TyTop) -> 
       true
   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
   | (TyRecord(fS), TyRecord(fT)) ->
       List.for_all
         (fun (li,tyTi) -> 
            try let tySi = List.assoc li fS in
                subtype tySi tyTi
            with Not_found -> false)
         fT
   | (_,_) -> 
       false

exception NoMeet

let labelunion ks ls =
    let rec nub xs = match xs with
                    [] -> []
                  | (li,_)::xs' -> li :: List.filter (fun kj -> li != kj) (nub xs) in
    nub (ks @ ls)

let rec join tyS tyT =
    match (tyS, tyT) with
      (TyBool,TyBool) ->
         TyBool
    | (TyArr(tyS1,tyS2), TyArr(tyT1,tyT2)) ->
         (try let m = meet tyS1 tyS2 in
              let j = join tyS2 tyT2 in
              TyArr(m, j)
         with NoMeet -> TyTop)
    | (TyRecord(fieldsS), TyRecord(fieldsT)) ->
         let memberT li = List.exists (fun (kj,_) -> li = kj) fieldsT in
         let typeT li = List.assoc li fieldsT in
         let commonFields = List.filter (fun (li,_) -> memberT li) fieldsS in
         TyRecord(List.map (fun (li,tySi) -> (li, join tySi (typeT li))) commonFields)
    | _ ->
         TyTop

and meet tyS tyT =
    match (tyS, tyT) with
      (TyTop,_) ->
         tyT
    | (_,TyTop) ->
         tyS
    | (TyBool,TyBool) ->
         TyBool
    | (TyArr(tyS1,tyS2), TyArr(tyT1,tyT2)) ->
         let j = join tyS1 tyS2 in
         let m = meet tyS2 tyT2 in
         TyArr(j,m)
    | (TyRecord(fieldsS), TyRecord(fieldsT)) ->
         let assoc' x xs = try Some(List.assoc x xs) with _ -> None in
         let ms = labelunion fieldsS fieldsT in
         let fieldTy m = (match (assoc' m fieldsS, assoc' m fieldsT) with
            (Some(tySi), Some(tyTi)) -> meet tySi tyTi
          | (Some(tySi), None) -> tySi
          | (None, Some(tyTi)) -> tyTi
          | (None, None) -> assert false) in
         TyRecord(List.map (fun m -> (m, fieldTy m)) ms)
    | _ ->
         raise NoMeet


(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
          TyArr(tyT11,tyT12) ->
            if subtype tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmRecord(fi, fields) ->
      let fieldtys = 
        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match (typeof ctx t1) with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi,t1,t2,t3) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      let tyT3 = typeof ctx t3 in
      (match tyT1 with
          TyBool -> join tyT2 tyT3
        | _ -> error fi "Expected bool in if condition")
