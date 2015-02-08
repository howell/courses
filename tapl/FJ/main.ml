(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Syntax
open Core

type program = classdef list * term

let fst : field = ("fst", TyObj "Object")
let snd : field = ("snd", TyObj "Object")

let setfst : methd =
    { mname    = "setfst";
      retTy    = TyObj "Pair";
      mformals = [("newfst", TyObj "Object")];
      mbody    = TmNew("Pair", [TmVar("newfst");
                                TmProj(TmVar("this"), "snd")]);
    }

let aClass : classdef =
    { cname = "A";
      super = "Object";
      cfields =  [];
      ctor = { ctorformals = []; supers = []; ctorfields = []; };
      methods = [];
    }

let bClass : classdef =
    { cname = "B";
      super = "Object";
      cfields =  [];
      ctor = { ctorformals = []; supers = []; ctorfields = []; };
      methods = [];
    }

let pairClass : classdef =
    { cname = "Pair";
      super = "Object";
      cfields =  [fst; snd];
      ctor = { ctorformals = [fst; snd];
               supers = [];
               ctorfields = [fst; snd];
             };
      methods = [setfst];
    }

let ex1 =
    let t0 = TmNew("Pair", [TmNew("A", []); TmNew("B", [])]) in
    TmInvk(t0, "setfst", [TmNew("B", [])])

let progTm =
    let innerPr = TmNew("Pair", [TmNew("A", []); TmNew("B", [])]) in
    let outerPr = TmNew("Pair", [innerPr; TmNew("A", [])]) in
    let projd = TmProj(outerPr, "fst") in
    let cast = TmCast(TyObj "Pair", projd) in
    TmCast(TyObj "B", TmProj(cast, "snd"))

let progClsses = addClasses [pairClass; aClass; bClass] emptyCT

let runprogram (classes,tm) =
    if List.for_all (classOk classes) classes
    then
        let ctx = emptycontext in
        let tmTy = typeof classes ctx tm in
        let res = eval classes ctx tm in
        printtm ctx res;
        pr " : ";
        printty ctx tmTy;
        print_newline()
    else
        print_string "Noooo\n"

let main () =
    let _ = runprogram (progClsses, progTm) in
    ()

let () = main ()

(*

open Support.Error

exception Bail

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | VarBind(tyT) -> VarBind(tyT)
  | TmAbbBind(t,None) -> TmAbbBind(t, Some(typeof ctx t))
  | TmAbbBind(t,Some(tyT)) ->
     let tyT' = typeof ctx t in
     if subtype ctx tyT' tyT then TmAbbBind(t,Some(tyT))
     else error fi "Type of binding does not match declared type"
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(tyT)

let prbindingty ctx b = match b with
    NameBind -> ()
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT 
  | TmAbbBind(t, tyT_opt) -> pr ": ";
     (match tyT_opt with
         None -> printty ctx (typeof ctx t)
       | Some(tyT) -> printty ctx tyT)
  | TyAbbBind(tyT) -> pr ":: *"

let rec process_command ctx cmd = match cmd with
  | Eval(fi,t) -> 
      let tyT = typeof ctx t in
      let t' = eval ctx t in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline();
      ctx
  | Bind(fi,x,bind) -> 
          raise Bail
  
let process_file f ctx =
  alreadyImported := f :: !alreadyImported;
  let cmds,_ = parseFile f ctx in
  let g ctx c =  
    open_hvbox 0;
    let results = process_command ctx c in
    print_flush();
    results
  in
    List.fold_left g ctx cmds

let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile emptycontext in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
*)
