type token =
  | TYPE of (Support.Error.info)
  | INERT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | LET of (Support.Error.info)
  | IN of (Support.Error.info)
  | FIX of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | USTRING of (Support.Error.info)
  | UNIT of (Support.Error.info)
  | UUNIT of (Support.Error.info)
  | AS of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | UFLOAT of (Support.Error.info)
  | REC of (Support.Error.info)
  | FOLD of (Support.Error.info)
  | UNFOLD of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | NAT of (Support.Error.info)
  | CASE of (Support.Error.info)
  | OF of (Support.Error.info)
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
# 80 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* TYPE *);
  258 (* INERT *);
  259 (* LAMBDA *);
  260 (* LET *);
  261 (* IN *);
  262 (* FIX *);
  263 (* LETREC *);
  264 (* IF *);
  265 (* THEN *);
  266 (* ELSE *);
  267 (* TRUE *);
  268 (* FALSE *);
  269 (* USTRING *);
  270 (* UNIT *);
  271 (* UUNIT *);
  272 (* AS *);
  273 (* BOOL *);
  274 (* TIMESFLOAT *);
  275 (* UFLOAT *);
  276 (* REC *);
  277 (* FOLD *);
  278 (* UNFOLD *);
  279 (* SUCC *);
  280 (* PRED *);
  281 (* ISZERO *);
  282 (* NAT *);
  283 (* CASE *);
  284 (* OF *);
  285 (* UCID *);
  286 (* LCID *);
  287 (* INTV *);
  288 (* FLOATV *);
  289 (* STRINGV *);
  290 (* APOSTROPHE *);
  291 (* DQUOTE *);
  292 (* ARROW *);
  293 (* BANG *);
  294 (* BARGT *);
  295 (* BARRCURLY *);
  296 (* BARRSQUARE *);
  297 (* COLON *);
  298 (* COLONCOLON *);
  299 (* COLONEQ *);
  300 (* COLONHASH *);
  301 (* COMMA *);
  302 (* DARROW *);
  303 (* DDARROW *);
  304 (* DOT *);
    0 (* EOF *);
  305 (* EQ *);
  306 (* EQEQ *);
  307 (* EXISTS *);
  308 (* GT *);
  309 (* HASH *);
  310 (* LCURLY *);
  311 (* LCURLYBAR *);
  312 (* LEFTARROW *);
  313 (* LPAREN *);
  314 (* LSQUARE *);
  315 (* LSQUAREBAR *);
  316 (* LT *);
  317 (* RCURLY *);
  318 (* RPAREN *);
  319 (* RSQUARE *);
  320 (* SEMI *);
  321 (* SLASH *);
  322 (* STAR *);
  323 (* TRIANGLE *);
  324 (* USCORE *);
  325 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\005\000\005\000\006\000\
\006\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\004\000\004\000\007\000\007\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\013\000\
\013\000\012\000\012\000\012\000\009\000\009\000\015\000\015\000\
\016\000\016\000\017\000\017\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\018\000\
\018\000\019\000\019\000\020\000\020\000\011\000\011\000\021\000\
\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\002\000\002\000\002\000\002\000\001\000\
\004\000\003\000\001\000\001\000\001\000\003\000\001\000\001\000\
\001\000\003\000\000\000\002\000\003\000\001\000\001\000\006\000\
\006\000\006\000\006\000\008\000\006\000\004\000\001\000\002\000\
\002\000\003\000\004\000\004\000\002\000\002\000\002\000\003\000\
\001\000\003\000\003\000\001\000\000\000\001\000\001\000\003\000\
\003\000\001\000\001\000\003\000\003\000\004\000\001\000\001\000\
\001\000\001\000\001\000\003\000\001\000\001\000\007\000\000\000\
\001\000\001\000\003\000\003\000\001\000\001\000\003\000\007\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\000\057\000\059\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\062\000\061\000\058\000\001\000\
\000\000\000\000\000\000\073\000\000\000\003\000\000\000\000\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\005\000\000\000\
\069\000\000\000\065\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\015\000\016\000\000\000\
\017\000\011\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\006\000\007\000\000\000\060\000\000\000\
\000\000\053\000\000\000\002\000\042\000\043\000\040\000\000\000\
\000\000\050\000\000\000\046\000\000\000\000\000\000\000\054\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\036\000\000\000\030\000\000\000\068\000\067\000\052\000\000\000\
\000\000\000\000\014\000\000\000\010\000\018\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\049\000\048\000\024\000\025\000\026\000\027\000\000\000\
\029\000\000\000\071\000\000\000\000\000\000\000\063\000\028\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\028\000\029\000\030\000\052\000\055\000\106\000\079\000\080\000\
\107\000\031\000\123\000\032\000\033\000\034\000\108\000\109\000\
\062\000\058\000\059\000\060\000\124\000"

let yysindex = "\006\000\
\001\000\000\000\217\254\228\254\231\254\224\001\250\254\108\001\
\000\000\000\000\000\000\224\001\226\254\235\254\224\001\224\001\
\224\001\108\001\237\254\219\254\000\000\000\000\000\000\000\000\
\149\001\108\001\009\255\000\000\236\254\000\000\224\001\255\254\
\000\000\036\255\016\255\012\255\014\255\005\255\007\255\000\000\
\255\254\018\255\051\255\186\001\016\255\016\255\255\254\255\254\
\255\254\033\255\016\255\000\000\016\255\108\001\000\000\015\255\
\000\000\006\255\000\000\023\255\008\255\021\255\022\255\001\000\
\255\254\240\254\016\255\000\000\000\000\000\000\000\000\055\255\
\000\000\000\000\245\001\016\255\245\001\017\255\000\000\049\255\
\016\255\016\255\108\001\108\001\016\255\108\001\255\254\024\255\
\025\255\030\255\000\000\000\000\000\000\108\001\000\000\149\001\
\108\001\000\000\108\001\000\000\000\000\000\000\000\000\047\255\
\056\255\000\000\035\255\000\000\053\255\037\255\048\255\000\000\
\253\001\058\255\059\255\096\255\108\255\065\255\106\255\000\000\
\000\000\087\255\000\000\054\255\000\000\000\000\000\000\066\255\
\016\255\016\255\000\000\245\001\000\000\000\000\000\000\108\001\
\108\001\108\001\108\001\108\001\108\001\076\255\030\255\112\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\124\255\
\000\000\100\255\000\000\016\255\108\001\093\255\000\000\000\000\
\092\255\190\001\224\001"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\082\255\250\000\000\000\000\000\000\000\000\000\
\086\255\000\000\000\000\000\000\000\000\000\000\041\255\184\255\
\000\000\173\255\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\095\000\106\000\
\167\000\000\000\000\000\000\000\000\000\000\000\000\000\048\001\
\000\000\000\000\000\000\088\255\091\255\000\000\000\000\000\000\
\178\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\089\255\000\000\102\255\000\000\000\000\110\255\
\000\000\000\000\000\000\000\000\000\000\000\000\239\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\212\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\099\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\255"

let yygindex = "\000\000\
\084\000\000\000\251\255\000\000\000\000\237\255\043\000\000\000\
\088\000\004\000\025\000\250\255\000\000\000\000\037\000\000\000\
\079\000\000\000\081\000\000\000\000\000"

let yytablesize = 825
let yytable = "\041\000\
\024\000\036\000\043\000\053\000\038\000\044\000\001\000\047\000\
\047\000\048\000\049\000\054\000\050\000\101\000\102\000\078\000\
\047\000\072\000\035\000\057\000\061\000\072\000\072\000\042\000\
\065\000\088\000\089\000\045\000\068\000\051\000\069\000\091\000\
\070\000\092\000\071\000\072\000\046\000\087\000\063\000\037\000\
\072\000\073\000\039\000\064\000\074\000\023\000\066\000\103\000\
\093\000\023\000\023\000\067\000\081\000\083\000\082\000\084\000\
\110\000\072\000\085\000\086\000\090\000\114\000\115\000\094\000\
\072\000\118\000\095\000\096\000\023\000\075\000\099\000\097\000\
\076\000\072\000\072\000\077\000\072\000\116\000\117\000\112\000\
\119\000\072\000\098\000\104\000\113\000\023\000\120\000\121\000\
\125\000\122\000\057\000\061\000\023\000\128\000\129\000\131\000\
\130\000\132\000\133\000\134\000\138\000\023\000\023\000\070\000\
\023\000\136\000\137\000\070\000\070\000\145\000\146\000\022\000\
\139\000\140\000\022\000\141\000\142\000\144\000\022\000\022\000\
\022\000\022\000\143\000\022\000\154\000\022\000\070\000\156\000\
\157\000\158\000\148\000\149\000\150\000\151\000\152\000\153\000\
\159\000\022\000\162\000\022\000\022\000\022\000\022\000\070\000\
\161\000\019\000\064\000\100\000\066\000\045\000\070\000\160\000\
\051\000\045\000\022\000\135\000\065\000\022\000\022\000\070\000\
\070\000\022\000\070\000\022\000\111\000\163\000\022\000\155\000\
\147\000\022\000\022\000\022\000\022\000\022\000\041\000\127\000\
\126\000\041\000\022\000\000\000\000\000\041\000\041\000\041\000\
\041\000\031\000\041\000\000\000\031\000\000\000\000\000\000\000\
\031\000\031\000\031\000\031\000\000\000\031\000\000\000\000\000\
\041\000\000\000\041\000\041\000\041\000\041\000\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\031\000\031\000\031\000\
\031\000\041\000\000\000\000\000\041\000\000\000\000\000\000\000\
\041\000\000\000\041\000\000\000\031\000\041\000\000\000\000\000\
\041\000\041\000\041\000\031\000\041\000\031\000\000\000\000\000\
\031\000\041\000\000\000\031\000\031\000\031\000\000\000\031\000\
\000\000\000\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\005\000\000\000\006\000\007\000\
\008\000\000\000\000\000\009\000\010\000\000\000\011\000\000\000\
\000\000\000\000\012\000\000\000\000\000\013\000\014\000\015\000\
\016\000\017\000\000\000\018\000\000\000\019\000\020\000\021\000\
\022\000\023\000\000\000\033\000\000\000\000\000\033\000\000\000\
\000\000\000\000\033\000\033\000\033\000\033\000\000\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\026\000\000\000\000\000\027\000\033\000\000\000\033\000\
\033\000\033\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\000\000\033\000\
\000\000\000\000\033\000\000\000\000\000\033\000\033\000\033\000\
\037\000\033\000\000\000\037\000\000\000\000\000\033\000\037\000\
\037\000\037\000\037\000\038\000\037\000\000\000\038\000\000\000\
\000\000\000\000\038\000\038\000\038\000\038\000\000\000\038\000\
\000\000\000\000\037\000\000\000\037\000\037\000\037\000\037\000\
\000\000\000\000\000\000\000\000\000\000\038\000\000\000\038\000\
\038\000\038\000\038\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\000\000\037\000\000\000\038\000\037\000\
\000\000\000\000\037\000\037\000\037\000\038\000\037\000\038\000\
\000\000\000\000\038\000\037\000\000\000\038\000\038\000\038\000\
\039\000\038\000\000\000\039\000\000\000\000\000\038\000\039\000\
\039\000\039\000\039\000\032\000\039\000\000\000\032\000\000\000\
\000\000\000\000\032\000\032\000\032\000\032\000\000\000\032\000\
\000\000\000\000\039\000\000\000\039\000\039\000\039\000\039\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\032\000\
\032\000\032\000\032\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\000\000\039\000\000\000\032\000\039\000\
\000\000\000\000\039\000\039\000\039\000\032\000\039\000\032\000\
\000\000\000\000\032\000\039\000\000\000\032\000\032\000\032\000\
\034\000\032\000\000\000\034\000\000\000\000\000\032\000\034\000\
\034\000\034\000\034\000\055\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\000\055\000\000\000\055\000\
\000\000\055\000\034\000\000\000\034\000\034\000\034\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\
\055\000\055\000\055\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\034\000\000\000\000\000\034\000\
\000\000\055\000\034\000\034\000\034\000\000\000\034\000\055\000\
\000\000\055\000\055\000\034\000\000\000\055\000\000\000\000\000\
\000\000\055\000\055\000\055\000\000\000\055\000\000\000\055\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\000\055\000\055\000\
\055\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\000\000\055\000\
\000\000\000\000\000\000\000\000\000\000\055\000\000\000\000\000\
\055\000\000\000\000\000\055\000\055\000\003\000\004\000\005\000\
\000\000\006\000\007\000\008\000\000\000\000\000\009\000\010\000\
\000\000\011\000\000\000\000\000\000\000\012\000\000\000\000\000\
\013\000\014\000\015\000\016\000\017\000\000\000\018\000\000\000\
\000\000\040\000\021\000\022\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\004\000\
\005\000\000\000\006\000\007\000\008\000\000\000\000\000\009\000\
\010\000\025\000\011\000\000\000\026\000\000\000\012\000\027\000\
\000\000\013\000\014\000\015\000\016\000\017\000\000\000\018\000\
\000\000\000\000\056\000\021\000\022\000\023\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\006\000\009\000\010\000\000\000\011\000\
\009\000\010\000\025\000\011\000\000\000\026\000\000\000\012\000\
\027\000\000\000\013\000\014\000\015\000\016\000\017\000\040\000\
\021\000\022\000\023\000\040\000\021\000\022\000\023\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\066\000\009\000\010\000\000\000\011\000\000\000\025\000\
\000\000\000\000\026\000\025\000\000\000\027\000\026\000\000\000\
\000\000\027\000\000\000\000\000\000\000\040\000\021\000\022\000\
\023\000\068\000\000\000\069\000\000\000\070\000\000\000\071\000\
\072\000\068\000\000\000\069\000\000\000\070\000\073\000\071\000\
\000\000\074\000\105\000\000\000\000\000\025\000\073\000\000\000\
\026\000\074\000\000\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\075\000\000\000\000\000\076\000\000\000\000\000\
\077\000\000\000\075\000\000\000\000\000\076\000\000\000\000\000\
\077\000"

let yycheck = "\006\000\
\000\000\030\001\008\000\041\001\030\001\012\000\001\000\052\001\
\015\000\016\000\017\000\049\001\018\000\030\001\031\001\035\000\
\061\001\005\001\058\001\025\000\026\000\009\001\010\001\030\001\
\031\000\045\000\046\000\058\001\013\001\049\001\015\001\051\000\
\017\001\053\000\019\001\020\001\058\001\044\000\030\001\068\001\
\028\001\026\001\068\001\064\001\029\001\005\001\048\001\067\000\
\054\000\009\001\010\001\016\001\041\001\049\001\041\001\049\001\
\076\000\045\001\041\001\009\001\028\001\081\000\082\000\049\001\
\052\001\085\000\061\001\045\001\028\001\054\001\049\001\064\001\
\057\001\061\001\062\001\060\001\064\001\083\000\084\000\063\001\
\086\000\069\001\062\001\029\001\036\001\045\001\063\001\063\001\
\094\000\060\001\096\000\097\000\052\001\099\000\048\001\061\001\
\041\001\045\001\062\001\052\001\005\001\061\001\062\001\005\001\
\064\001\048\001\048\001\009\001\010\001\129\000\130\000\002\001\
\005\001\049\001\005\001\010\001\030\001\052\001\009\001\010\001\
\011\001\012\001\069\001\014\001\049\001\016\001\028\001\016\001\
\005\001\030\001\136\000\137\000\138\000\139\000\140\000\141\000\
\156\000\028\001\047\001\030\001\031\001\032\001\033\001\045\001\
\052\001\064\001\061\001\064\000\061\001\061\001\052\001\157\000\
\062\001\052\001\045\001\113\000\163\000\048\001\049\001\061\001\
\062\001\052\001\064\001\054\001\077\000\162\000\057\001\143\000\
\132\000\060\001\061\001\062\001\063\001\064\001\002\001\097\000\
\096\000\005\001\069\001\255\255\255\255\009\001\010\001\011\001\
\012\001\002\001\014\001\255\255\005\001\255\255\255\255\255\255\
\009\001\010\001\011\001\012\001\255\255\014\001\255\255\255\255\
\028\001\255\255\030\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\255\255\028\001\255\255\030\001\031\001\032\001\
\033\001\045\001\255\255\255\255\048\001\255\255\255\255\255\255\
\052\001\255\255\054\001\255\255\045\001\057\001\255\255\255\255\
\060\001\061\001\062\001\052\001\064\001\054\001\255\255\255\255\
\057\001\069\001\255\255\060\001\061\001\062\001\255\255\064\001\
\255\255\255\255\255\255\255\255\069\001\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\255\255\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\255\255\014\001\255\255\
\255\255\255\255\018\001\255\255\255\255\021\001\022\001\023\001\
\024\001\025\001\255\255\027\001\255\255\029\001\030\001\031\001\
\032\001\033\001\255\255\002\001\255\255\255\255\005\001\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\014\001\
\255\255\255\255\255\255\255\255\255\255\255\255\054\001\255\255\
\255\255\057\001\255\255\255\255\060\001\028\001\255\255\030\001\
\031\001\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\045\001\255\255\
\255\255\255\255\255\255\255\255\255\255\052\001\255\255\054\001\
\255\255\255\255\057\001\255\255\255\255\060\001\061\001\062\001\
\002\001\064\001\255\255\005\001\255\255\255\255\069\001\009\001\
\010\001\011\001\012\001\002\001\014\001\255\255\005\001\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\014\001\
\255\255\255\255\028\001\255\255\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\028\001\255\255\030\001\
\031\001\032\001\033\001\045\001\255\255\255\255\255\255\255\255\
\255\255\255\255\052\001\255\255\054\001\255\255\045\001\057\001\
\255\255\255\255\060\001\061\001\062\001\052\001\064\001\054\001\
\255\255\255\255\057\001\069\001\255\255\060\001\061\001\062\001\
\002\001\064\001\255\255\005\001\255\255\255\255\069\001\009\001\
\010\001\011\001\012\001\002\001\014\001\255\255\005\001\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\014\001\
\255\255\255\255\028\001\255\255\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\028\001\255\255\030\001\
\031\001\032\001\033\001\045\001\255\255\255\255\255\255\255\255\
\255\255\255\255\052\001\255\255\054\001\255\255\045\001\057\001\
\255\255\255\255\060\001\061\001\062\001\052\001\064\001\054\001\
\255\255\255\255\057\001\069\001\255\255\060\001\061\001\062\001\
\002\001\064\001\255\255\005\001\255\255\255\255\069\001\009\001\
\010\001\011\001\012\001\002\001\014\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\255\255\014\001\
\255\255\016\001\028\001\255\255\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\030\001\
\031\001\032\001\033\001\045\001\255\255\255\255\255\255\255\255\
\255\255\255\255\052\001\255\255\054\001\255\255\255\255\057\001\
\255\255\048\001\060\001\061\001\062\001\255\255\064\001\054\001\
\255\255\002\001\057\001\069\001\255\255\060\001\255\255\255\255\
\255\255\064\001\011\001\012\001\255\255\014\001\255\255\016\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\030\001\031\001\032\001\
\033\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\045\001\255\255\255\255\048\001\
\255\255\255\255\255\255\255\255\255\255\054\001\255\255\255\255\
\057\001\255\255\255\255\060\001\061\001\002\001\003\001\004\001\
\255\255\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\255\255\014\001\255\255\255\255\255\255\018\001\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\255\255\027\001\255\255\
\255\255\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\002\001\003\001\
\004\001\255\255\006\001\007\001\008\001\255\255\255\255\011\001\
\012\001\054\001\014\001\255\255\057\001\255\255\018\001\060\001\
\255\255\021\001\022\001\023\001\024\001\025\001\255\255\027\001\
\255\255\255\255\030\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\255\255\002\001\255\255\255\255\255\255\002\001\
\255\255\255\255\255\255\006\001\011\001\012\001\255\255\014\001\
\011\001\012\001\054\001\014\001\255\255\057\001\255\255\018\001\
\060\001\255\255\021\001\022\001\023\001\024\001\025\001\030\001\
\031\001\032\001\033\001\030\001\031\001\032\001\033\001\255\255\
\255\255\002\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\048\001\011\001\012\001\255\255\014\001\255\255\054\001\
\255\255\255\255\057\001\054\001\255\255\060\001\057\001\255\255\
\255\255\060\001\255\255\255\255\255\255\030\001\031\001\032\001\
\033\001\013\001\255\255\015\001\255\255\017\001\255\255\019\001\
\020\001\013\001\255\255\015\001\255\255\017\001\026\001\019\001\
\255\255\029\001\030\001\255\255\255\255\054\001\026\001\255\255\
\057\001\029\001\255\255\060\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\054\001\255\255\255\255\057\001\255\255\255\255\
\060\001\255\255\054\001\255\255\255\255\057\001\255\255\255\255\
\060\001"

let yynames_const = "\
  "

let yynames_block = "\
  TYPE\000\
  INERT\000\
  LAMBDA\000\
  LET\000\
  IN\000\
  FIX\000\
  LETREC\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  USTRING\000\
  UNIT\000\
  UUNIT\000\
  AS\000\
  BOOL\000\
  TIMESFLOAT\000\
  UFLOAT\000\
  REC\000\
  FOLD\000\
  UNFOLD\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  NAT\000\
  CASE\000\
  OF\000\
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
# 127 "parser.mly"
      ( fun ctx -> [],ctx )
# 556 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 129 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 568 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 137 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 575 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 139 "parser.mly"
      ( fun ctx -> ((Bind(_1.i, _1.v, _2 ctx)), addname ctx _1.v) )
# 583 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 141 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 591 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 146 "parser.mly"
      ( fun ctx -> VarBind (_2 ctx))
# 599 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 148 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 607 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 153 "parser.mly"
                ( _1 )
# 614 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 155 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TyRec(_2.v,_4 ctx1) )
# 626 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 162 "parser.mly"
           ( _2 )
# 635 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 164 "parser.mly"
      ( fun ctx ->
          if isnamebound ctx _1.v then
            TyVar(name2index _1.i ctx _1.v, ctxlength ctx)
          else 
            TyId(_1.v) )
# 646 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 170 "parser.mly"
      ( fun ctx -> TyString )
# 653 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 172 "parser.mly"
      ( fun ctx -> TyUnit )
# 660 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'FieldTypes) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 174 "parser.mly"
      ( fun ctx ->
          TyRecord(_2 ctx 1) )
# 670 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 177 "parser.mly"
      ( fun ctx -> TyBool )
# 677 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 179 "parser.mly"
      ( fun ctx -> TyFloat )
# 684 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 181 "parser.mly"
      ( fun ctx -> TyNat )
# 691 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'FieldTypes) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 183 "parser.mly"
      ( fun ctx ->
          TyVariant(_2 ctx 1) )
# 701 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "parser.mly"
      ( fun ctx -> TyVarBind )
# 707 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 190 "parser.mly"
      ( fun ctx -> TyAbbBind(_2 ctx) )
# 715 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 196 "parser.mly"
     ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 724 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 198 "parser.mly"
            ( _1 )
# 731 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 202 "parser.mly"
      ( _1 )
# 738 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 204 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 752 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 208 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 766 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 212 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 778 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 214 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 790 "parser.ml"
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
# 216 "parser.mly"
      ( fun ctx -> 
          let ctx1 = addname ctx _2.v in 
          TmLet(_1, _2.v, TmFix(_1, TmAbs(_1, _2.v, _4 ctx, _6 ctx1)),
                _8 ctx1) )
# 807 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 221 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 819 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Cases) in
    Obj.repr(
# 223 "parser.mly"
      ( fun ctx ->
          TmCase(_1, _2 ctx, _4 ctx) )
# 830 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 228 "parser.mly"
      ( _1 )
# 837 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 230 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 848 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 235 "parser.mly"
      ( fun ctx ->
          TmFix(_1, _2 ctx) )
# 857 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 238 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 866 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 240 "parser.mly"
      ( fun ctx -> TmFold(_1,_3 ctx))
# 876 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 242 "parser.mly"
      ( fun ctx -> TmUnfold(_1,_3 ctx))
# 886 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 244 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 894 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 246 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 902 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 248 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 910 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 252 "parser.mly"
      ( fun ctx -> TmAscribe(_2, _1 ctx, _3 ctx) )
# 919 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 254 "parser.mly"
      ( _1 )
# 926 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 258 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, _3.v) )
# 936 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 261 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 946 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 264 "parser.mly"
      ( _1 )
# 953 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 268 "parser.mly"
      ( fun ctx i -> [] )
# 959 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 270 "parser.mly"
      ( _1 )
# 966 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FieldType) in
    Obj.repr(
# 274 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 973 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'FieldType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 276 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 982 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 280 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 991 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 282 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 998 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 286 "parser.mly"
      ( _1 )
# 1005 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TermSeq) in
    Obj.repr(
# 288 "parser.mly"
      ( fun ctx ->
          TmApp(_2, TmAbs(_2, "_", TyUnit, _3 (addname ctx "_")), _1 ctx) )
# 1015 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TermSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 294 "parser.mly"
      ( _2 )
# 1024 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 296 "parser.mly"
      ( fun ctx -> TmInert(_1, _3 ctx) )
# 1034 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 298 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 1042 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 301 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 1049 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 303 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 1056 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 305 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 1063 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 307 "parser.mly"
      ( fun ctx -> TmUnit(_1) )
# 1070 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 309 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 1080 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 312 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 1087 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 314 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 1098 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 320 "parser.mly"
      ( fun ctx ->
          TmTag(_1, _2.v, _4 ctx, _7 ctx) )
# 1112 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 325 "parser.mly"
      ( fun ctx i -> [] )
# 1118 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 327 "parser.mly"
      ( _1 )
# 1125 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 331 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 1132 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 333 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 1141 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 337 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 1150 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 339 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 1157 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Case) in
    Obj.repr(
# 343 "parser.mly"
      ( fun ctx -> [_1 ctx] )
# 1164 "parser.ml"
               : 'Cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Case) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Cases) in
    Obj.repr(
# 345 "parser.mly"
      ( fun ctx -> (_1 ctx) :: (_3 ctx) )
# 1173 "parser.ml"
               : 'Cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string Support.Error.withinfo) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 349 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _4.v in
          (_2.v, (_4.v, _7 ctx1)) )
# 1188 "parser.ml"
               : 'Case))
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
