(* Exercises 22.3.10  and 22.4.6*)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyId of string

type term =
    TmTrue
  | TmFalse
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmIf of term * term * term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term

type constr = (ty * ty) list

let addConstr t1 t2 cs = (t1, t2) :: cs

let emptyConstrs = []

type tysubst = string * ty
type tysubsts = tysubst list

let emptySubsts = []

let rec substType typ subst = match (subst, typ) with
    (_, TyBool) -> TyBool
  | (_, TyNat)  -> TyNat
  | (_, TyArr(ty1,ty2)) ->
          TyArr(substType ty1 subst, substType ty2 subst)
  | ((x,tyX), TyId(y)) ->
          if x = y
          then tyX
          else typ

let substTypes typ substs =
    List.fold_left substType typ substs

let substConstrs constrs substs =
    List.map (fun (s,t) -> ((substType s substs), (substType t substs))) constrs

let compSubsts subs1 subs2 =
    let subs2' = List.map (fun (x,tyX) -> (x, substTypes tyX subs1)) subs2 in
    let insubs2 x = List.exists (fun (y,_) -> x = y) subs2 in
    let subs1' = List.filter (fun (x,_) -> not (insubs2 x)) subs1 in
    subs2' @ subs1'

let rec fv typ = match typ with
    TyBool -> []
  | TyNat -> []
  | TyArr(TyId(x),ty2) -> List.filter (fun y -> x != y) (fv ty2)
  | TyArr(ty1,ty2) -> fv ty1 @ fv ty2
  | TyId(x) -> [x]

let rec unify constrs = match constrs with
    [] -> Some emptySubsts
  | (tyS,tyT) :: constrs' ->
          if tyS = tyT
          then unify constrs'
          else (match (tyS,tyT) with
                (TyId(x), _) when not (List.mem x (fv tyT)) ->
                    let subst = (x, tyT) in
                    (match unify (substConstrs constrs' subst) with
                        None -> None
                      | Some(subs) -> Some (compSubsts subs [subst]))
              | (_, TyId(x)) when not (List.mem x (fv tyS)) ->
                    let subst = (x, tyS) in
                    (match unify (substConstrs constrs' subst) with
                        None -> None
                      | Some(subs) -> Some (compSubsts subs [subst]))
              | (TyArr(tyS1,tyS2), TyArr(tyT1,tyT2)) ->
                      unify (addConstr tyS1 tyT1 (addConstr tyS2 tyT2 constrs'))
              | _ ->
                      None)

type nextuvar = NextUVar of string * uvargenerator
and uvargenerator = unit -> nextuvar

let uvargen =
    let rec f n () = NextUVar("?X_" ^ string_of_int n, f (n + 1))
    in f 0

let uncons gen = match gen () with
    NextUVar(x, gen') -> (TyId x, gen')

type context = (string * ty) list

let emptycontext = []

let addToContext var typ ctx = (var, typ) :: ctx

let lookup var ctx = try Some(List.assoc var ctx)
                     with _ -> None

let rec typeof tm ctx gen = match tm with
    TmTrue -> (TyBool, gen, emptyConstrs)
  | TmFalse -> (TyBool, gen, emptyConstrs)
  | TmZero -> (TyNat, gen, emptyConstrs)
  | TmSucc(t1) ->
          let (tyT1, gen', constrs) = typeof t1 ctx gen
          in (TyNat, gen', addConstr tyT1 TyNat constrs)
  | TmPred(t1) ->
          let (tyT1, gen', constrs) = typeof t1 ctx gen
          in (TyNat, gen', addConstr tyT1 TyNat constrs)
  | TmIsZero(t1) ->
          let (tyT1, gen', constrs) = typeof t1 ctx gen
          in (TyBool, gen', addConstr tyT1 TyNat constrs)
  | TmIf(t1,t2,t3) ->
          let (tyT1, gen1, constrs1) = typeof t1 ctx gen
          in let (tyT2, gen2, constrs2) = typeof t2 ctx gen1
          in let (tyT3, gen3, constrs3) = typeof t3 ctx gen2
          in let constrs = constrs1 @ constrs2 @ constrs3
          in let constrs1 = addConstr tyT1 TyBool constrs
          in let constrs2 = addConstr tyT2 tyT3 constrs1
          in (match tyT1 with
            TyBool -> (tyT2, gen3, constrs2)
          | _ -> print_string "expected bool in if"; assert false)
  | TmVar(x) ->
          (match lookup x ctx with
            Some(tyT) -> (tyT, gen, emptyConstrs)
          | None      -> print_string ("Variable " ^ x ^ " lookup failed");
                         assert false)
  | TmAbs(x,ty1,t1) ->
          let ctx' = addToContext x ty1 ctx
          in let (ty2, gen', constrs) = typeof t1 ctx' gen
          in (TyArr(ty1,ty2), gen', constrs)
  | TmApp(t1,t2) ->
          let (tyT1, gen1, constrs1) = typeof t1 ctx gen in
          let (tyT2, gen2, constrs2) = typeof t2 ctx gen1 in
          let (x, gen') = uncons gen in
          let constrs = addConstr tyT1 (TyArr(tyT2,x)) (constrs1 @ constrs2) in
          (x, gen', constrs)

let rec showTy ty = match ty with
    TyBool       -> "Bool"
  | TyNat        -> "Nat"
  | TyArr(t1,t2) -> "(" ^ showTy t1 ^ " -> " ^ showTy t2 ^ ")"
  | TyId(x)      -> x

let showConstr cs =
    let p (t1, t2) = showTy t1 ^ " = " ^ showTy t2 in
    let rec loop cs = match cs with
        [] -> ""
      | cstr :: [] -> p cstr
      | cstr :: cs' -> p cstr ^ ", "  ^ loop cs' in
    "{" ^ loop cs ^ "}"

let showSubsts subs =
    let p (x, tyX) = x ^ " |-> " ^ showTy tyX in
    let rec loop subs = match subs with
        [] -> ""
      | sub :: [] -> p sub
      | sub :: subs' -> p sub ^ ", "  ^ loop subs' in
    "[" ^ loop subs ^ "]"

let test = TmApp(TmAbs("x", TyNat, TmSucc(TmSucc(TmVar("x")))), TmZero)

let main () = let (tyT, _, constrs) = typeof test emptycontext uvargen in
              print_string (showTy tyT);
              print_string "\n";
              print_string (showConstr constrs);
              print_string "\n";
              (match unify constrs with
                None -> print_string "Failed to Unify!"
              | Some(subs) ->
                    print_string (showSubsts subs);
                    print_string "\n";
                    let uniTy = substTypes tyT subs in
                    print_string (showTy uniTy));
              print_string "\n"

let () = main ()
