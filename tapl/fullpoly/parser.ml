type token =
  | TYPE of (Support.Error.info)
  | INERT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | FIX of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | USTRING of (Support.Error.info)
  | UNIT of (Support.Error.info)
  | UUNIT of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | UFLOAT of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | NAT of (Support.Error.info)
  | SOME of (Support.Error.info)
  | LET of (Support.Error.info)
  | IN of (Support.Error.info)
  | AS of (Support.Error.info)
  | ALL of (Support.Error.info)
  | UCID of (string Support.Error.withinfo)
  | LCID of (string Support.Error.withinfo)
  | INTV of (int Support.Error.withinfo)
  | FLOATV of (float Support.Error.withinfo)
  | STRINGV of (string Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 7 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
# 77 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* TYPE *);
  258 (* INERT *);
  259 (* LAMBDA *);
  260 (* FIX *);
  261 (* LETREC *);
  262 (* USTRING *);
  263 (* UNIT *);
  264 (* UUNIT *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* BOOL *);
  271 (* TIMESFLOAT *);
  272 (* UFLOAT *);
  273 (* SUCC *);
  274 (* PRED *);
  275 (* ISZERO *);
  276 (* NAT *);
  277 (* SOME *);
  278 (* LET *);
  279 (* IN *);
  280 (* AS *);
  281 (* ALL *);
  282 (* UCID *);
  283 (* LCID *);
  284 (* INTV *);
  285 (* FLOATV *);
  286 (* STRINGV *);
  287 (* APOSTROPHE *);
  288 (* DQUOTE *);
  289 (* ARROW *);
  290 (* BANG *);
  291 (* BARGT *);
  292 (* BARRCURLY *);
  293 (* BARRSQUARE *);
  294 (* COLON *);
  295 (* COLONCOLON *);
  296 (* COLONEQ *);
  297 (* COLONHASH *);
  298 (* COMMA *);
  299 (* DARROW *);
  300 (* DDARROW *);
  301 (* DOT *);
    0 (* EOF *);
  302 (* EQ *);
  303 (* EQEQ *);
  304 (* EXISTS *);
  305 (* GT *);
  306 (* HASH *);
  307 (* LCURLY *);
  308 (* LCURLYBAR *);
  309 (* LEFTARROW *);
  310 (* LPAREN *);
  311 (* LSQUARE *);
  312 (* LSQUAREBAR *);
  313 (* LT *);
  314 (* RCURLY *);
  315 (* RPAREN *);
  316 (* RSQUARE *);
  317 (* SEMI *);
  318 (* SLASH *);
  319 (* STAR *);
  320 (* TRIANGLE *);
  321 (* USCORE *);
  322 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\005\000\005\000\
\006\000\006\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\004\000\004\000\007\000\007\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\012\000\012\000\011\000\011\000\011\000\009\000\009\000\014\000\
\014\000\015\000\015\000\016\000\016\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\017\000\017\000\018\000\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\002\000\002\000\007\000\002\000\002\000\
\001\000\004\000\003\000\001\000\001\000\001\000\003\000\001\000\
\001\000\001\000\006\000\000\000\002\000\003\000\001\000\001\000\
\006\000\006\000\006\000\006\000\008\000\006\000\010\000\004\000\
\001\000\002\000\002\000\003\000\002\000\002\000\002\000\004\000\
\003\000\001\000\003\000\003\000\001\000\000\000\001\000\001\000\
\003\000\003\000\001\000\001\000\003\000\003\000\004\000\001\000\
\001\000\001\000\003\000\001\000\001\000\001\000\001\000\008\000\
\000\000\001\000\001\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\000\000\000\
\060\000\061\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\063\000\062\000\057\000\001\000\000\000\000\000\071\000\
\000\000\003\000\000\000\000\000\045\000\000\000\000\000\000\000\
\000\000\000\000\056\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\000\000\000\000\005\000\000\000\000\000\000\000\070\000\000\000\
\066\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\014\000\016\000\017\000\018\000\000\000\012\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\007\000\
\008\000\000\000\000\000\000\000\059\000\000\000\000\000\054\000\
\002\000\000\000\043\000\044\000\041\000\000\000\000\000\000\000\
\051\000\000\000\047\000\000\000\000\000\055\000\000\000\032\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\069\000\000\000\068\000\053\000\040\000\000\000\000\000\000\000\
\015\000\000\000\011\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\000\000\050\000\
\049\000\025\000\026\000\000\000\030\000\027\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\019\000\
\029\000\000\000\064\000\000\000\031\000"

let yydgoto = "\002\000\
\024\000\025\000\055\000\048\000\051\000\105\000\076\000\077\000\
\106\000\027\000\028\000\029\000\030\000\107\000\108\000\060\000\
\056\000\057\000\058\000"

let yysindex = "\007\000\
\001\000\000\000\217\254\235\254\096\255\246\254\000\000\181\001\
\000\000\000\000\096\255\096\255\096\255\096\255\239\254\229\254\
\221\254\000\000\000\000\000\000\000\000\155\255\181\001\000\000\
\216\254\000\000\089\255\234\254\000\000\000\255\151\001\248\254\
\253\254\001\255\000\000\185\255\234\254\003\255\028\255\059\255\
\234\254\234\254\234\254\252\254\020\255\008\255\151\001\000\000\
\151\001\181\001\000\000\013\255\012\255\151\001\000\000\004\255\
\000\000\022\255\238\254\006\255\001\000\151\001\234\254\241\254\
\151\001\000\000\000\000\000\000\000\000\000\000\041\255\000\000\
\217\255\151\001\009\255\000\000\035\255\181\001\151\001\151\001\
\151\001\181\001\234\254\181\001\031\255\181\001\000\000\000\000\
\000\000\043\255\181\001\032\255\000\000\211\001\181\001\000\000\
\000\000\019\255\000\000\000\000\000\000\037\255\054\255\046\255\
\000\000\034\255\000\000\051\255\036\255\000\000\243\001\000\000\
\055\255\060\255\061\255\095\255\088\255\085\255\092\255\062\255\
\000\000\181\001\000\000\000\000\000\000\151\001\080\255\151\001\
\000\000\228\001\000\000\000\000\181\001\181\001\181\001\181\001\
\181\001\069\255\181\001\082\255\071\255\000\000\151\001\000\000\
\000\000\000\000\000\000\108\255\000\000\000\000\091\255\000\000\
\181\001\115\255\083\255\181\001\181\001\000\000\151\001\000\000\
\000\000\119\255\000\000\181\001\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\084\255\
\002\255\000\000\000\000\000\000\000\000\090\255\000\000\000\000\
\000\000\000\000\017\255\122\000\000\000\085\000\000\000\000\000\
\000\000\000\000\000\000\090\255\159\000\000\000\000\000\000\000\
\196\000\233\000\014\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\124\001\000\000\000\000\000\000\
\000\000\093\255\087\255\000\000\000\000\000\000\051\001\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\103\255\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\088\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\107\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\088\000\000\000\255\255\000\000\000\000\227\255\042\000\000\000\
\000\000\000\000\240\000\000\000\000\000\024\000\000\000\071\000\
\000\000\075\000\000\000"

let yytablesize = 809
let yytable = "\026\000\
\021\000\075\000\049\000\056\000\032\000\033\000\039\000\001\000\
\056\000\044\000\050\000\099\000\100\000\056\000\056\000\031\000\
\038\000\087\000\047\000\088\000\061\000\059\000\064\000\065\000\
\092\000\056\000\024\000\024\000\056\000\056\000\056\000\056\000\
\098\000\045\000\079\000\101\000\078\000\082\000\080\000\024\000\
\081\000\084\000\095\000\034\000\109\000\085\000\056\000\046\000\
\089\000\113\000\114\000\115\000\056\000\086\000\090\000\056\000\
\056\000\091\000\024\000\026\000\003\000\093\000\056\000\094\000\
\096\000\007\000\102\000\111\000\110\000\120\000\009\000\010\000\
\118\000\122\000\024\000\024\000\112\000\024\000\125\000\127\000\
\116\000\126\000\117\000\128\000\119\000\035\000\018\000\019\000\
\020\000\121\000\003\000\129\000\130\000\059\000\131\000\007\000\
\142\000\003\000\144\000\133\000\009\000\010\000\007\000\064\000\
\134\000\136\000\135\000\009\000\010\000\036\000\137\000\138\000\
\023\000\155\000\139\000\035\000\018\000\019\000\020\000\140\000\
\141\000\143\000\035\000\018\000\019\000\020\000\151\000\153\000\
\154\000\163\000\156\000\146\000\147\000\148\000\149\000\150\000\
\157\000\152\000\159\000\036\000\160\000\164\000\023\000\062\000\
\020\000\052\000\036\000\065\000\097\000\023\000\067\000\158\000\
\132\000\145\000\161\000\162\000\003\000\004\000\005\000\006\000\
\046\000\007\000\165\000\008\000\048\000\124\000\009\000\010\000\
\123\000\011\000\000\000\012\000\013\000\014\000\000\000\000\000\
\015\000\000\000\000\000\000\000\052\000\053\000\018\000\019\000\
\020\000\000\000\003\000\004\000\005\000\006\000\000\000\007\000\
\000\000\008\000\000\000\000\000\009\000\010\000\000\000\011\000\
\000\000\012\000\013\000\014\000\000\000\036\000\015\000\000\000\
\023\000\000\000\000\000\053\000\018\000\019\000\020\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\066\000\000\000\
\067\000\000\000\000\000\000\000\000\000\000\000\068\000\000\000\
\069\000\000\000\000\000\036\000\070\000\103\000\023\000\000\000\
\000\000\071\000\072\000\104\000\037\000\000\000\000\000\054\000\
\000\000\000\000\040\000\041\000\042\000\043\000\000\000\000\000\
\000\000\000\000\003\000\004\000\005\000\006\000\000\000\007\000\
\000\000\008\000\063\000\073\000\009\000\010\000\074\000\011\000\
\000\000\012\000\013\000\014\000\000\000\000\000\015\000\083\000\
\000\000\000\000\016\000\017\000\018\000\019\000\020\000\023\000\
\000\000\000\000\000\000\000\000\023\000\000\000\000\000\023\000\
\023\000\023\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\023\000\023\000\023\000\000\000\
\023\000\023\000\023\000\023\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\000\000\023\000\023\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\023\000\023\000\000\000\042\000\023\000\
\023\000\023\000\023\000\042\000\000\000\000\000\042\000\042\000\
\042\000\042\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\042\000\000\000\000\000\000\000\042\000\
\042\000\042\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\000\000\000\000\042\000\000\000\
\033\000\042\000\000\000\033\000\033\000\033\000\033\000\042\000\
\000\000\000\000\042\000\042\000\000\000\000\000\042\000\042\000\
\033\000\042\000\000\000\000\000\033\000\033\000\033\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\000\000\000\000\033\000\000\000\035\000\000\000\000\000\
\035\000\035\000\035\000\035\000\033\000\000\000\000\000\033\000\
\033\000\000\000\000\000\033\000\033\000\035\000\033\000\000\000\
\000\000\035\000\035\000\035\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\035\000\000\000\037\000\000\000\000\000\037\000\037\000\037\000\
\037\000\035\000\000\000\000\000\035\000\035\000\000\000\000\000\
\035\000\035\000\037\000\035\000\000\000\000\000\037\000\037\000\
\037\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\000\000\000\000\037\000\000\000\038\000\
\000\000\000\000\038\000\038\000\038\000\038\000\037\000\000\000\
\000\000\037\000\037\000\000\000\000\000\037\000\037\000\038\000\
\037\000\000\000\000\000\038\000\038\000\038\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\000\000\000\000\038\000\000\000\039\000\000\000\000\000\039\000\
\039\000\039\000\039\000\038\000\000\000\000\000\038\000\038\000\
\000\000\000\000\038\000\038\000\039\000\038\000\000\000\000\000\
\039\000\039\000\039\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\000\000\039\000\
\000\000\034\000\000\000\000\000\034\000\034\000\034\000\034\000\
\039\000\000\000\000\000\039\000\039\000\000\000\000\000\039\000\
\039\000\034\000\039\000\000\000\000\000\034\000\034\000\034\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\034\000\000\000\036\000\000\000\
\000\000\036\000\036\000\036\000\036\000\034\000\000\000\000\000\
\034\000\034\000\000\000\000\000\034\000\034\000\036\000\034\000\
\000\000\000\000\036\000\036\000\036\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\056\000\000\000\000\000\
\000\000\036\000\056\000\000\000\000\000\000\000\000\000\056\000\
\056\000\000\000\036\000\000\000\000\000\036\000\036\000\000\000\
\000\000\036\000\036\000\056\000\036\000\000\000\056\000\056\000\
\056\000\056\000\000\000\000\000\066\000\000\000\067\000\000\000\
\000\000\000\000\000\000\000\000\068\000\056\000\069\000\000\000\
\056\000\000\000\070\000\000\000\000\000\000\000\056\000\071\000\
\072\000\056\000\056\000\000\000\000\000\056\000\003\000\004\000\
\005\000\006\000\000\000\007\000\000\000\008\000\000\000\000\000\
\009\000\010\000\000\000\011\000\000\000\012\000\013\000\014\000\
\000\000\073\000\015\000\000\000\074\000\000\000\000\000\035\000\
\018\000\019\000\020\000\000\000\003\000\004\000\005\000\006\000\
\000\000\007\000\000\000\008\000\000\000\000\000\009\000\010\000\
\000\000\011\000\000\000\012\000\013\000\014\000\000\000\036\000\
\015\000\066\000\023\000\067\000\000\000\053\000\018\000\019\000\
\020\000\068\000\000\000\069\000\000\000\000\000\000\000\070\000\
\066\000\000\000\067\000\000\000\071\000\072\000\104\000\000\000\
\068\000\000\000\069\000\000\000\000\000\036\000\070\000\000\000\
\023\000\000\000\000\000\000\000\072\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\073\000\000\000\
\000\000\074\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\073\000\000\000\000\000\
\074\000"

let yycheck = "\001\000\
\000\000\031\000\038\001\002\001\026\001\027\001\008\000\001\000\
\007\001\027\001\046\001\027\001\028\001\012\001\013\001\055\001\
\027\001\047\000\046\001\049\000\061\001\023\000\045\001\024\001\
\054\000\024\001\010\001\011\001\027\001\028\001\029\001\030\001\
\062\000\051\001\038\001\065\000\045\001\010\001\038\001\023\001\
\038\001\046\001\061\001\065\001\074\000\026\001\045\001\065\001\
\050\000\079\000\080\000\081\000\051\001\046\001\042\001\054\001\
\055\001\046\001\042\001\061\000\002\001\058\001\061\001\042\001\
\059\001\007\001\026\001\033\001\060\001\027\001\012\001\013\001\
\042\001\042\001\058\001\059\001\078\000\061\001\060\001\026\001\
\082\000\045\001\084\000\038\001\086\000\027\001\028\001\029\001\
\030\001\091\000\002\001\058\001\042\001\095\000\059\001\007\001\
\126\000\002\001\128\000\045\001\012\001\013\001\007\001\045\001\
\045\001\011\001\046\001\012\001\013\001\051\001\023\001\027\001\
\054\001\143\000\023\001\027\001\028\001\029\001\030\001\058\001\
\122\000\042\001\027\001\028\001\029\001\030\001\058\001\046\001\
\058\001\159\000\023\001\133\000\134\000\135\000\136\000\137\000\
\046\001\139\000\024\001\051\001\058\001\023\001\054\001\055\001\
\061\001\059\001\051\001\058\001\061\000\054\001\058\001\153\000\
\111\000\130\000\156\000\157\000\002\001\003\001\004\001\005\001\
\058\001\007\001\164\000\009\001\058\001\095\000\012\001\013\001\
\094\000\015\001\255\255\017\001\018\001\019\001\255\255\255\255\
\022\001\255\255\255\255\255\255\026\001\027\001\028\001\029\001\
\030\001\255\255\002\001\003\001\004\001\005\001\255\255\007\001\
\255\255\009\001\255\255\255\255\012\001\013\001\255\255\015\001\
\255\255\017\001\018\001\019\001\255\255\051\001\022\001\255\255\
\054\001\255\255\255\255\027\001\028\001\029\001\030\001\255\255\
\255\255\063\001\255\255\255\255\255\255\255\255\006\001\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\014\001\255\255\
\016\001\255\255\255\255\051\001\020\001\021\001\054\001\255\255\
\255\255\025\001\026\001\027\001\005\000\255\255\255\255\063\001\
\255\255\255\255\011\000\012\000\013\000\014\000\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\255\255\007\001\
\255\255\009\001\027\000\051\001\012\001\013\001\054\001\015\001\
\255\255\017\001\018\001\019\001\255\255\255\255\022\001\040\000\
\255\255\255\255\026\001\027\001\028\001\029\001\030\001\002\001\
\255\255\255\255\255\255\255\255\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\051\001\023\001\024\001\054\001\255\255\
\027\001\028\001\029\001\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\042\001\
\255\255\255\255\045\001\046\001\255\255\255\255\255\255\255\255\
\051\001\255\255\255\255\054\001\055\001\255\255\002\001\058\001\
\059\001\060\001\061\001\007\001\255\255\255\255\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\023\001\255\255\255\255\255\255\027\001\
\028\001\029\001\030\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\002\001\255\255\255\255\042\001\255\255\
\007\001\045\001\255\255\010\001\011\001\012\001\013\001\051\001\
\255\255\255\255\054\001\055\001\255\255\255\255\058\001\059\001\
\023\001\061\001\255\255\255\255\027\001\028\001\029\001\030\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\002\001\255\255\255\255\042\001\255\255\007\001\255\255\255\255\
\010\001\011\001\012\001\013\001\051\001\255\255\255\255\054\001\
\055\001\255\255\255\255\058\001\059\001\023\001\061\001\255\255\
\255\255\027\001\028\001\029\001\030\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\002\001\255\255\255\255\
\042\001\255\255\007\001\255\255\255\255\010\001\011\001\012\001\
\013\001\051\001\255\255\255\255\054\001\055\001\255\255\255\255\
\058\001\059\001\023\001\061\001\255\255\255\255\027\001\028\001\
\029\001\030\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\042\001\255\255\007\001\
\255\255\255\255\010\001\011\001\012\001\013\001\051\001\255\255\
\255\255\054\001\055\001\255\255\255\255\058\001\059\001\023\001\
\061\001\255\255\255\255\027\001\028\001\029\001\030\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\001\
\255\255\255\255\042\001\255\255\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\051\001\255\255\255\255\054\001\055\001\
\255\255\255\255\058\001\059\001\023\001\061\001\255\255\255\255\
\027\001\028\001\029\001\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\002\001\255\255\255\255\042\001\
\255\255\007\001\255\255\255\255\010\001\011\001\012\001\013\001\
\051\001\255\255\255\255\054\001\055\001\255\255\255\255\058\001\
\059\001\023\001\061\001\255\255\255\255\027\001\028\001\029\001\
\030\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\002\001\255\255\255\255\042\001\255\255\007\001\255\255\
\255\255\010\001\011\001\012\001\013\001\051\001\255\255\255\255\
\054\001\055\001\255\255\255\255\058\001\059\001\023\001\061\001\
\255\255\255\255\027\001\028\001\029\001\030\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\002\001\255\255\255\255\
\255\255\042\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\255\255\051\001\255\255\255\255\054\001\055\001\255\255\
\255\255\058\001\059\001\024\001\061\001\255\255\027\001\028\001\
\029\001\030\001\255\255\255\255\006\001\255\255\008\001\255\255\
\255\255\255\255\255\255\255\255\014\001\042\001\016\001\255\255\
\045\001\255\255\020\001\255\255\255\255\255\255\051\001\025\001\
\026\001\054\001\055\001\255\255\255\255\058\001\002\001\003\001\
\004\001\005\001\255\255\007\001\255\255\009\001\255\255\255\255\
\012\001\013\001\255\255\015\001\255\255\017\001\018\001\019\001\
\255\255\051\001\022\001\255\255\054\001\255\255\255\255\027\001\
\028\001\029\001\030\001\255\255\002\001\003\001\004\001\005\001\
\255\255\007\001\255\255\009\001\255\255\255\255\012\001\013\001\
\255\255\015\001\255\255\017\001\018\001\019\001\255\255\051\001\
\022\001\006\001\054\001\008\001\255\255\027\001\028\001\029\001\
\030\001\014\001\255\255\016\001\255\255\255\255\255\255\020\001\
\006\001\255\255\008\001\255\255\025\001\026\001\027\001\255\255\
\014\001\255\255\016\001\255\255\255\255\051\001\020\001\255\255\
\054\001\255\255\255\255\255\255\026\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\051\001\255\255\
\255\255\054\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\051\001\255\255\255\255\
\054\001"

let yynames_const = "\
  "

let yynames_block = "\
  TYPE\000\
  INERT\000\
  LAMBDA\000\
  FIX\000\
  LETREC\000\
  USTRING\000\
  UNIT\000\
  UUNIT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  BOOL\000\
  TIMESFLOAT\000\
  UFLOAT\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  NAT\000\
  SOME\000\
  LET\000\
  IN\000\
  AS\000\
  ALL\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 124 "parser.mly"
      ( fun ctx -> [],ctx )
# 541 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 126 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 553 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 134 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 560 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 136 "parser.mly"
      ( fun ctx -> ((Bind(_1.i, _1.v, _2 ctx)), addname ctx _1.v) )
# 568 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 138 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 576 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string Support.Error.withinfo) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 140 "parser.mly"
     ( fun ctx ->
         let ctx1 = addname ctx _2.v in
         let ctx2 = addname ctx1 _4.v in
         (SomeBind(_1,_2.v,_4.v,_7 ctx), ctx2) )
# 592 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 148 "parser.mly"
      ( fun ctx -> VarBind (_2 ctx))
# 600 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 150 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 608 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 155 "parser.mly"
                ( _1 )
# 615 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 157 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TyAll(_2.v,_4 ctx1) )
# 627 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 164 "parser.mly"
           ( _2 )
# 636 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 166 "parser.mly"
      ( fun ctx ->
          if isnamebound ctx _1.v then
            TyVar(name2index _1.i ctx _1.v, ctxlength ctx)
          else 
            TyId(_1.v) )
# 647 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 172 "parser.mly"
      ( fun ctx -> TyString )
# 654 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 174 "parser.mly"
      ( fun ctx -> TyUnit )
# 661 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'FieldTypes) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 176 "parser.mly"
      ( fun ctx ->
          TyRecord(_2 ctx 1) )
# 671 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 179 "parser.mly"
      ( fun ctx -> TyBool )
# 678 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 181 "parser.mly"
      ( fun ctx -> TyFloat )
# 685 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 183 "parser.mly"
      ( fun ctx -> TyNat )
# 692 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 185 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _3.v in
          TySome(_3.v, _5 ctx1) )
# 706 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "parser.mly"
      ( fun ctx -> TyVarBind )
# 712 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 193 "parser.mly"
      ( fun ctx -> TyAbbBind(_2 ctx) )
# 720 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 199 "parser.mly"
     ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 729 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 201 "parser.mly"
            ( _1 )
# 736 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 205 "parser.mly"
      ( _1 )
# 743 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 207 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 757 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 211 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 771 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 215 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 783 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 217 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 795 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 219 "parser.mly"
      ( fun ctx -> 
          let ctx1 = addname ctx _2.v in 
          TmLet(_1, _2.v, TmFix(_1, TmAbs(_1, _2.v, _4 ctx, _6 ctx1)),
                _8 ctx1) )
# 812 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 224 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 824 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 8 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string Support.Error.withinfo) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 226 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _3.v in
          let ctx2 = addname ctx1 _5.v in
          TmUnpack(_1,_3.v,_5.v,_8 ctx,_10 ctx2) )
# 843 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 231 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmTAbs(_1,_2.v,_4 ctx1) )
# 855 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 237 "parser.mly"
      ( _1 )
# 862 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 239 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 873 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 244 "parser.mly"
      ( fun ctx ->
          TmFix(_1, _2 ctx) )
# 882 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 247 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 891 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 249 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 899 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 251 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 907 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 253 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 915 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 255 "parser.mly"
      ( fun ctx ->
          let t1 = _1 ctx in
          let t2 = _3 ctx in
          TmTApp(tmInfo t1,t1,t2) )
# 928 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 262 "parser.mly"
      ( fun ctx -> TmAscribe(_2, _1 ctx, _3 ctx) )
# 937 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 264 "parser.mly"
      ( _1 )
# 944 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 268 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, _3.v) )
# 954 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 271 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 964 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 274 "parser.mly"
      ( _1 )
# 971 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 278 "parser.mly"
      ( fun ctx i -> [] )
# 977 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 280 "parser.mly"
      ( _1 )
# 984 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FieldType) in
    Obj.repr(
# 284 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 991 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'FieldType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 286 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 1000 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 290 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 1009 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 292 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 1016 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 296 "parser.mly"
      ( _1 )
# 1023 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TermSeq) in
    Obj.repr(
# 298 "parser.mly"
      ( fun ctx ->
          TmApp(_2, TmAbs(_2, "_", TyUnit, _3 (addname ctx "_")), _1 ctx) )
# 1033 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TermSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 304 "parser.mly"
      ( _2 )
# 1042 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 306 "parser.mly"
      ( fun ctx -> TmInert(_1, _3 ctx) )
# 1052 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 308 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 1060 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 311 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 1067 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 313 "parser.mly"
      ( fun ctx -> TmUnit(_1) )
# 1074 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 315 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 1084 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 318 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 1091 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 320 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 1098 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 322 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 1105 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 324 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 1116 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'Term) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 330 "parser.mly"
      ( fun ctx ->
          TmPack(_1,_3 ctx,_5 ctx,_8 ctx) )
# 1131 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 335 "parser.mly"
      ( fun ctx i -> [] )
# 1137 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 337 "parser.mly"
      ( _1 )
# 1144 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 341 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 1151 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 343 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 1160 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 347 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 1169 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 349 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 1176 "parser.ml"
               : 'Field))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.context -> (Syntax.command list * Syntax.context) )
