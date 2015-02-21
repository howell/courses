(* Exercise 22.3.10 *)

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

let  showConstr cs =
    let p (t1, t2) = showTy t1 ^ " = " ^ showTy t2 in
    let rec loop cs = match cs with
        [] -> ""
      | cstr :: [] -> p cstr
      | cstr :: cs' -> p cstr ^ ", "  ^ loop cs' in
    "{" ^ loop cs ^ "}"

let test = TmApp(TmAbs("x", TyNat, TmSucc(TmSucc(TmVar("x")))), TmZero)

let main () = let (tyT, _, constrs) = typeof test emptycontext uvargen in
              print_string (showTy tyT);
              print_string "\n";
              print_string (showConstr constrs);
              print_string "\n"

let () = main ()
