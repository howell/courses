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
exception NoCoercion
exception SubTypeExn of string

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

let rec transtype t =
    match t with
    TyTop -> TyUnit
  | TyArr(ty1,ty2) -> TyArr(transtype ty1, transtype ty2)
  | TyBool -> t
  | TyUnit -> t
  | TyRecord(fields) -> TyRecord(List.map (fun (li,ti) ->
          (li, transtype ti)) fields)

let dinf = dummyinfo

let rec subtype tyS tyT = let body =
   if (=) tyS tyT then TmVar(dinf,0,0) else
   match (tyS,tyT) with
     (_,TyTop) ->
       TmUnit(dinf)
   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       let c1 = subtype tyT1 tyS1 in
       let c2 = subtype tyS2 tyT2 in
       let coercearg = TmApp(dinf, c1, TmVar(dinf,0,0)) in
       let app_term = TmApp(dinf, TmVar(dinf,1,0), coercearg) in
       let coerceresult = TmApp(dinf, c2, app_term) in
       TmAbs(dinf, "y", transtype tyT1, coerceresult)
   | (TyRecord(fS), TyRecord(fT)) ->
       let fields = List.map (fun (li,tyTi) ->
            let tySi = try List.assoc li fS
                       with _ -> raise (SubTypeExn ("missing field " ^ li)) in
            let ci = subtype tySi tyTi in
            (li, TmApp(dinf, ci, TmProj(dinf, TmVar(dinf,0,0), li)))) fT in
       TmRecord(dinf, fields)
   | (_,_) ->
       raise (SubTypeExn "incompatible types") in
   TmAbs(dinf, "x", transtype tyS, body)

let rec join tyS tyT =
  (* Write me *) assert false

and meet tyS tyT =
  (* Write me *) assert false

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmVar(fi,i,_) -> (getTypeFromContext fi ctx i, t)
  | TmUnit(fi) -> (TyUnit, t)
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let (tyT2, tm2) = typeof ctx' t2 in
      (TyArr(tyT1, tyT2), TmAbs(fi,x, transtype tyT1, tm2))
  | TmApp(fi,t1,t2) ->
      let (tyT1, tm1) = typeof ctx t1 in
      let (tyT2, tm2) = typeof ctx t2 in
      (match tyT1 with
          TyArr(tyT11,tyT12) ->
            (try let c = subtype tyT2 tyT11 in
            (tyT12, TmApp(fi, tm1, TmApp(fi, c, tm2)))
            with SubTypeExn msg -> error fi ("parameter type mismatch: " ^ msg))
        | _ -> error fi "arrow type expected")
  | TmRecord(fi, fields) ->
      let fieldtys =
        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
      (TyRecord(List.map (fun (li, (ti,_)) -> (li, ti)) fieldtys),
       TmRecord(fi, List.map (fun (li, (_,tmi)) -> (li, tmi)) fieldtys))
  | TmProj(fi, t1, l) ->
      (match (typeof ctx t1) with
          (TyRecord(fieldtys), tm1) ->
            (try (List.assoc l fieldtys, TmProj(fi, tm1, l))
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmTrue(fi) ->
      (TyBool, t)
  | TmFalse(fi) ->
      (TyBool, t)
  | TmIf(fi,t1,t2,t3) ->
     let (tyT1, tm1) = typeof ctx t1 in
     if (=) tyT1 TyBool then
       let (tyT2, tm2) = typeof ctx t2 in
       let (tyT3, tm3) = typeof ctx t3 in
       if (=) tyT2 tyT3 then (tyT2, TmIf(fi, tm1, tm2, tm3))
       else error fi "arms of conditional have different types"
     else error fi "guard of conditional not a boolean"
