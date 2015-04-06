type token =
  | TYPE of (Support.Error.info)
  | INERT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | TTOP of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | FIX of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | USTRING of (Support.Error.info)
  | UNIT of (Support.Error.info)
  | UUNIT of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | UFLOAT of (Support.Error.info)
  | LEQ of (Support.Error.info)
  | ALL of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | NAT of (Support.Error.info)
  | SOME of (Support.Error.info)
  | LET of (Support.Error.info)
  | IN of (Support.Error.info)
  | AS of (Support.Error.info)
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
# 79 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* TYPE *);
  258 (* INERT *);
  259 (* LAMBDA *);
  260 (* TTOP *);
  261 (* IF *);
  262 (* THEN *);
  263 (* ELSE *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* BOOL *);
  267 (* FIX *);
  268 (* LETREC *);
  269 (* USTRING *);
  270 (* UNIT *);
  271 (* UUNIT *);
  272 (* TIMESFLOAT *);
  273 (* UFLOAT *);
  274 (* LEQ *);
  275 (* ALL *);
  276 (* SUCC *);
  277 (* PRED *);
  278 (* ISZERO *);
  279 (* NAT *);
  280 (* SOME *);
  281 (* LET *);
  282 (* IN *);
  283 (* AS *);
  284 (* UCID *);
  285 (* LCID *);
  286 (* INTV *);
  287 (* FLOATV *);
  288 (* STRINGV *);
  289 (* APOSTROPHE *);
  290 (* DQUOTE *);
  291 (* ARROW *);
  292 (* BANG *);
  293 (* BARGT *);
  294 (* BARRCURLY *);
  295 (* BARRSQUARE *);
  296 (* COLON *);
  297 (* COLONCOLON *);
  298 (* COLONEQ *);
  299 (* COLONHASH *);
  300 (* COMMA *);
  301 (* DARROW *);
  302 (* DDARROW *);
  303 (* DOT *);
    0 (* EOF *);
  304 (* EQ *);
  305 (* EQEQ *);
  306 (* EXISTS *);
  307 (* GT *);
  308 (* HASH *);
  309 (* LCURLY *);
  310 (* LCURLYBAR *);
  311 (* LEFTARROW *);
  312 (* LPAREN *);
  313 (* LSQUARE *);
  314 (* LSQUAREBAR *);
  315 (* LT *);
  316 (* RCURLY *);
  317 (* RPAREN *);
  318 (* RSQUARE *);
  319 (* SEMI *);
  320 (* SLASH *);
  321 (* STAR *);
  322 (* TRIANGLE *);
  323 (* USCORE *);
  324 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\005\000\005\000\
\006\000\006\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\004\000\004\000\004\000\007\000\
\007\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\012\000\010\000\010\000\014\000\
\014\000\015\000\015\000\013\000\013\000\017\000\017\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\018\000\018\000\019\000\019\000\020\000\020\000\
\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\002\000\002\000\007\000\002\000\002\000\
\001\000\005\000\003\000\001\000\001\000\001\000\003\000\001\000\
\001\000\001\000\001\000\007\000\000\000\002\000\002\000\003\000\
\001\000\001\000\006\000\006\000\006\000\006\000\006\000\008\000\
\005\000\010\000\001\000\002\000\002\000\003\000\004\000\002\000\
\002\000\002\000\003\000\003\000\001\000\000\000\001\000\001\000\
\003\000\003\000\001\000\003\000\001\000\001\000\003\000\003\000\
\004\000\001\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\008\000\000\000\001\000\001\000\003\000\003\000\001\000\
\000\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\059\000\060\000\000\000\
\000\000\063\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\065\000\064\000\062\000\001\000\000\000\000\000\075\000\
\000\000\003\000\000\000\000\000\045\000\000\000\000\000\000\000\
\000\000\000\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\005\000\000\000\000\000\000\000\072\000\
\000\000\068\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\014\000\016\000\017\000\018\000\000\000\
\019\000\012\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\023\000\007\000\008\000\000\000\000\000\000\000\
\061\000\000\000\000\000\056\000\002\000\000\000\043\000\044\000\
\052\000\000\000\000\000\000\000\051\000\000\000\047\000\000\000\
\000\000\057\000\000\000\074\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\071\000\000\000\070\000\
\055\000\039\000\000\000\000\000\000\000\015\000\000\000\011\000\
\024\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\049\000\027\000\
\028\000\029\000\000\000\030\000\000\000\031\000\000\000\000\000\
\010\000\000\000\000\000\000\000\006\000\000\000\000\000\032\000\
\000\000\066\000\020\000\000\000\034\000"

let yydgoto = "\002\000\
\024\000\025\000\056\000\049\000\052\000\109\000\078\000\081\000\
\079\000\110\000\027\000\028\000\029\000\111\000\112\000\030\000\
\061\000\057\000\058\000\059\000"

let yysindex = "\019\000\
\001\000\000\000\222\254\233\254\126\001\000\000\000\000\195\001\
\251\254\000\000\195\001\195\001\195\001\195\001\230\254\246\254\
\225\254\000\000\000\000\000\000\000\000\145\255\126\001\000\000\
\218\254\000\000\070\001\237\254\000\000\003\255\004\002\013\255\
\248\254\252\254\000\000\183\255\029\255\237\254\255\254\214\255\
\237\254\237\254\237\254\253\254\012\255\254\254\004\002\004\002\
\000\000\004\002\126\001\000\000\004\255\001\255\004\002\000\000\
\239\254\000\000\011\255\250\254\005\255\001\000\004\002\237\254\
\238\254\004\002\000\000\000\000\000\000\000\000\000\000\032\255\
\000\000\000\000\218\001\004\002\000\255\000\000\030\255\004\002\
\024\255\004\002\004\002\126\001\004\002\237\254\126\001\028\255\
\126\001\000\000\000\000\000\000\000\000\044\255\126\001\034\255\
\000\000\164\001\126\001\000\000\000\000\014\255\000\000\000\000\
\000\000\013\255\046\255\045\255\000\000\035\255\000\000\040\255\
\038\255\000\000\018\002\000\000\126\001\042\255\049\255\093\255\
\053\255\076\255\080\255\084\255\051\255\000\000\126\001\000\000\
\000\000\000\000\065\255\013\255\004\002\000\000\239\001\000\000\
\000\000\000\000\126\001\126\001\126\001\126\001\126\001\055\255\
\126\001\072\255\063\255\004\002\081\255\000\000\000\000\000\000\
\000\000\000\000\101\255\000\000\082\255\000\000\126\001\102\255\
\000\000\004\002\126\001\126\001\000\000\004\002\068\255\000\000\
\105\255\000\000\000\000\126\001\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\255\
\050\255\000\000\000\000\000\000\000\000\074\255\000\000\000\000\
\000\000\000\000\174\255\094\000\000\000\061\255\000\000\088\255\
\000\000\000\000\000\000\074\255\000\000\105\000\000\000\000\000\
\165\000\176\000\236\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\062\001\000\000\000\000\
\000\000\000\000\083\255\075\255\000\000\000\000\000\000\247\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\085\255\000\000\000\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\088\255\000\000\000\000\000\000\000\000\000\000\086\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\107\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\087\000\000\000\255\255\000\000\000\000\227\255\037\000\157\255\
\000\000\000\000\000\000\002\000\000\000\020\000\000\000\000\000\
\061\000\000\000\066\000\000\000"

let yytablesize = 842
let yytable = "\026\000\
\021\000\077\000\044\000\037\000\032\000\033\000\131\000\047\000\
\050\000\038\000\103\000\104\000\040\000\041\000\042\000\043\000\
\051\000\090\000\091\000\001\000\092\000\060\000\031\000\039\000\
\062\000\096\000\045\000\065\000\064\000\066\000\080\000\082\000\
\149\000\102\000\084\000\083\000\105\000\048\000\085\000\088\000\
\046\000\086\000\097\000\034\000\087\000\089\000\113\000\094\000\
\095\000\093\000\116\000\058\000\118\000\119\000\098\000\121\000\
\099\000\058\000\058\000\106\000\026\000\114\000\053\000\058\000\
\115\000\100\000\053\000\053\000\053\000\053\000\117\000\123\000\
\125\000\132\000\053\000\130\000\058\000\127\000\058\000\058\000\
\058\000\058\000\120\000\135\000\133\000\122\000\053\000\124\000\
\139\000\053\000\053\000\053\000\053\000\126\000\134\000\140\000\
\058\000\060\000\136\000\141\000\142\000\143\000\058\000\150\000\
\053\000\058\000\058\000\053\000\144\000\145\000\146\000\148\000\
\058\000\053\000\157\000\138\000\053\000\053\000\161\000\159\000\
\053\000\053\000\160\000\053\000\162\000\147\000\163\000\171\000\
\166\000\164\000\172\000\021\000\167\000\067\000\073\000\054\000\
\170\000\152\000\153\000\154\000\155\000\156\000\069\000\158\000\
\046\000\048\000\003\000\004\000\101\000\005\000\073\000\137\000\
\006\000\007\000\151\000\008\000\009\000\165\000\010\000\129\000\
\011\000\168\000\169\000\128\000\012\000\013\000\014\000\000\000\
\000\000\015\000\173\000\000\000\053\000\054\000\018\000\019\000\
\020\000\000\000\000\000\026\000\026\000\000\000\000\000\000\000\
\003\000\004\000\000\000\005\000\000\000\000\000\006\000\007\000\
\000\000\008\000\009\000\000\000\010\000\036\000\011\000\026\000\
\023\000\000\000\012\000\013\000\014\000\000\000\000\000\015\000\
\000\000\055\000\000\000\054\000\018\000\019\000\020\000\003\000\
\000\000\026\000\000\000\000\000\000\000\006\000\007\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\000\000\026\000\026\000\036\000\026\000\000\000\023\000\000\000\
\000\000\000\000\035\000\018\000\019\000\020\000\000\000\055\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\065\000\005\000\000\000\000\000\
\006\000\007\000\036\000\008\000\009\000\023\000\010\000\000\000\
\011\000\000\000\000\000\000\000\012\000\013\000\014\000\000\000\
\000\000\015\000\000\000\000\000\016\000\017\000\018\000\019\000\
\020\000\025\000\000\000\000\000\000\000\025\000\025\000\025\000\
\025\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\
\023\000\025\000\025\000\000\000\025\000\025\000\025\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\025\000\025\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\025\000\
\025\000\000\000\000\000\025\000\025\000\025\000\025\000\035\000\
\000\000\000\000\000\000\035\000\035\000\035\000\035\000\000\000\
\000\000\000\000\037\000\035\000\000\000\000\000\037\000\037\000\
\037\000\037\000\000\000\000\000\000\000\000\000\037\000\035\000\
\000\000\000\000\035\000\035\000\035\000\035\000\000\000\000\000\
\000\000\000\000\037\000\000\000\000\000\037\000\037\000\037\000\
\037\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\037\000\035\000\035\000\000\000\
\000\000\035\000\035\000\000\000\035\000\037\000\000\000\000\000\
\037\000\037\000\000\000\000\000\037\000\037\000\040\000\037\000\
\000\000\000\000\040\000\040\000\040\000\040\000\000\000\000\000\
\000\000\041\000\040\000\000\000\000\000\041\000\041\000\041\000\
\041\000\000\000\000\000\000\000\000\000\041\000\040\000\000\000\
\000\000\040\000\040\000\040\000\040\000\000\000\000\000\000\000\
\000\000\041\000\000\000\000\000\041\000\041\000\041\000\041\000\
\040\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\000\000\041\000\040\000\040\000\000\000\000\000\
\040\000\040\000\000\000\040\000\041\000\000\000\000\000\041\000\
\041\000\000\000\000\000\041\000\041\000\042\000\041\000\000\000\
\000\000\042\000\042\000\042\000\042\000\000\000\000\000\000\000\
\036\000\042\000\000\000\000\000\036\000\036\000\036\000\036\000\
\000\000\000\000\000\000\000\000\036\000\042\000\000\000\000\000\
\042\000\042\000\042\000\042\000\000\000\000\000\000\000\000\000\
\036\000\000\000\000\000\036\000\036\000\036\000\036\000\042\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000\000\000\036\000\042\000\042\000\000\000\000\000\042\000\
\042\000\000\000\042\000\036\000\000\000\000\000\036\000\036\000\
\000\000\000\000\036\000\036\000\038\000\036\000\000\000\000\000\
\038\000\038\000\038\000\038\000\000\000\000\000\000\000\058\000\
\038\000\000\000\000\000\000\000\000\000\058\000\058\000\003\000\
\000\000\000\000\000\000\058\000\038\000\006\000\007\000\038\000\
\038\000\038\000\038\000\010\000\000\000\000\000\000\000\000\000\
\058\000\000\000\058\000\058\000\058\000\058\000\038\000\000\000\
\000\000\000\000\035\000\018\000\019\000\020\000\000\000\038\000\
\000\000\058\000\038\000\038\000\058\000\000\000\038\000\038\000\
\000\000\038\000\058\000\000\000\000\000\058\000\058\000\000\000\
\000\000\058\000\036\000\000\000\000\000\023\000\063\000\003\000\
\004\000\000\000\005\000\000\000\000\000\006\000\007\000\000\000\
\008\000\009\000\000\000\010\000\000\000\011\000\000\000\000\000\
\000\000\012\000\013\000\014\000\000\000\000\000\015\000\000\000\
\000\000\000\000\035\000\018\000\019\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\000\004\000\000\000\
\005\000\000\000\000\000\006\000\007\000\000\000\008\000\009\000\
\000\000\010\000\036\000\011\000\000\000\023\000\000\000\012\000\
\013\000\014\000\000\000\000\000\015\000\000\000\000\000\000\000\
\054\000\018\000\019\000\020\000\003\000\000\000\000\000\000\000\
\000\000\000\000\006\000\007\000\000\000\000\000\000\000\000\000\
\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\000\000\000\000\023\000\000\000\067\000\000\000\035\000\
\018\000\019\000\020\000\068\000\000\000\000\000\069\000\000\000\
\070\000\000\000\071\000\000\000\072\000\000\000\000\000\000\000\
\073\000\107\000\067\000\000\000\000\000\074\000\108\000\036\000\
\068\000\000\000\023\000\069\000\000\000\070\000\000\000\071\000\
\000\000\072\000\000\000\000\000\000\000\073\000\000\000\067\000\
\000\000\000\000\074\000\108\000\000\000\068\000\075\000\000\000\
\069\000\076\000\070\000\000\000\071\000\067\000\072\000\000\000\
\000\000\000\000\073\000\068\000\000\000\000\000\069\000\074\000\
\070\000\000\000\071\000\075\000\000\000\000\000\076\000\000\000\
\073\000\000\000\000\000\000\000\000\000\074\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\075\000\000\000\000\000\076\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\000\000\000\
\000\000\076\000"

let yycheck = "\001\000\
\000\000\031\000\029\001\005\000\028\001\029\001\106\000\018\001\
\040\001\008\000\029\001\030\001\011\000\012\000\013\000\014\000\
\048\001\047\000\048\000\001\000\050\000\023\000\057\001\029\001\
\063\001\055\000\053\001\047\001\027\000\027\001\018\001\040\001\
\132\000\063\000\006\001\040\001\066\000\048\001\040\001\028\001\
\067\001\040\000\060\001\067\001\048\001\048\001\076\000\044\001\
\048\001\051\000\080\000\002\001\082\000\083\000\044\001\085\000\
\063\001\008\001\009\001\028\001\062\000\062\001\002\001\014\001\
\035\001\061\001\006\001\007\001\008\001\009\001\047\001\044\001\
\029\001\028\001\014\001\062\001\027\001\044\001\029\001\030\001\
\031\001\032\001\084\000\044\001\040\001\087\000\026\001\089\000\
\047\001\029\001\030\001\031\001\032\001\095\000\060\001\047\001\
\047\001\099\000\061\001\007\001\048\001\026\001\053\001\133\000\
\044\001\056\001\057\001\047\001\029\001\026\001\060\001\047\001\
\063\001\053\001\060\001\117\000\056\001\057\001\148\000\048\001\
\060\001\061\001\060\001\063\001\044\001\127\000\026\001\060\001\
\027\001\048\001\026\001\063\001\162\000\060\001\047\001\061\001\
\166\000\139\000\140\000\141\000\142\000\143\000\060\001\145\000\
\060\001\060\001\002\001\003\001\062\000\005\001\044\001\115\000\
\008\001\009\001\135\000\011\001\012\001\159\000\014\001\099\000\
\016\001\163\000\164\000\098\000\020\001\021\001\022\001\255\255\
\255\255\025\001\172\000\255\255\028\001\029\001\030\001\031\001\
\032\001\255\255\255\255\006\001\007\001\255\255\255\255\255\255\
\002\001\003\001\255\255\005\001\255\255\255\255\008\001\009\001\
\255\255\011\001\012\001\255\255\014\001\053\001\016\001\026\001\
\056\001\255\255\020\001\021\001\022\001\255\255\255\255\025\001\
\255\255\065\001\255\255\029\001\030\001\031\001\032\001\002\001\
\255\255\044\001\255\255\255\255\255\255\008\001\009\001\255\255\
\255\255\255\255\255\255\014\001\255\255\255\255\255\255\255\255\
\255\255\060\001\061\001\053\001\063\001\255\255\056\001\255\255\
\255\255\255\255\029\001\030\001\031\001\032\001\255\255\065\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\047\001\005\001\255\255\255\255\
\008\001\009\001\053\001\011\001\012\001\056\001\014\001\255\255\
\016\001\255\255\255\255\255\255\020\001\021\001\022\001\255\255\
\255\255\025\001\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\002\001\255\255\255\255\255\255\006\001\007\001\008\001\
\009\001\255\255\255\255\255\255\255\255\014\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\053\001\255\255\255\255\
\056\001\026\001\027\001\255\255\029\001\030\001\031\001\032\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\044\001\255\255\255\255\047\001\048\001\
\255\255\255\255\255\255\255\255\053\001\255\255\255\255\056\001\
\057\001\255\255\255\255\060\001\061\001\062\001\063\001\002\001\
\255\255\255\255\255\255\006\001\007\001\008\001\009\001\255\255\
\255\255\255\255\002\001\014\001\255\255\255\255\006\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\014\001\026\001\
\255\255\255\255\029\001\030\001\031\001\032\001\255\255\255\255\
\255\255\255\255\026\001\255\255\255\255\029\001\030\001\031\001\
\032\001\044\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\053\001\255\255\044\001\056\001\057\001\255\255\
\255\255\060\001\061\001\255\255\063\001\053\001\255\255\255\255\
\056\001\057\001\255\255\255\255\060\001\061\001\002\001\063\001\
\255\255\255\255\006\001\007\001\008\001\009\001\255\255\255\255\
\255\255\002\001\014\001\255\255\255\255\006\001\007\001\008\001\
\009\001\255\255\255\255\255\255\255\255\014\001\026\001\255\255\
\255\255\029\001\030\001\031\001\032\001\255\255\255\255\255\255\
\255\255\026\001\255\255\255\255\029\001\030\001\031\001\032\001\
\044\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\053\001\255\255\044\001\056\001\057\001\255\255\255\255\
\060\001\061\001\255\255\063\001\053\001\255\255\255\255\056\001\
\057\001\255\255\255\255\060\001\061\001\002\001\063\001\255\255\
\255\255\006\001\007\001\008\001\009\001\255\255\255\255\255\255\
\002\001\014\001\255\255\255\255\006\001\007\001\008\001\009\001\
\255\255\255\255\255\255\255\255\014\001\026\001\255\255\255\255\
\029\001\030\001\031\001\032\001\255\255\255\255\255\255\255\255\
\026\001\255\255\255\255\029\001\030\001\031\001\032\001\044\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\044\001\056\001\057\001\255\255\255\255\060\001\
\061\001\255\255\063\001\053\001\255\255\255\255\056\001\057\001\
\255\255\255\255\060\001\061\001\002\001\063\001\255\255\255\255\
\006\001\007\001\008\001\009\001\255\255\255\255\255\255\002\001\
\014\001\255\255\255\255\255\255\255\255\008\001\009\001\002\001\
\255\255\255\255\255\255\014\001\026\001\008\001\009\001\029\001\
\030\001\031\001\032\001\014\001\255\255\255\255\255\255\255\255\
\027\001\255\255\029\001\030\001\031\001\032\001\044\001\255\255\
\255\255\255\255\029\001\030\001\031\001\032\001\255\255\053\001\
\255\255\044\001\056\001\057\001\047\001\255\255\060\001\061\001\
\255\255\063\001\053\001\255\255\255\255\056\001\057\001\255\255\
\255\255\060\001\053\001\255\255\255\255\056\001\057\001\002\001\
\003\001\255\255\005\001\255\255\255\255\008\001\009\001\255\255\
\011\001\012\001\255\255\014\001\255\255\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\255\255\
\255\255\255\255\029\001\030\001\031\001\032\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\002\001\003\001\255\255\
\005\001\255\255\255\255\008\001\009\001\255\255\011\001\012\001\
\255\255\014\001\053\001\016\001\255\255\056\001\255\255\020\001\
\021\001\022\001\255\255\255\255\025\001\255\255\255\255\255\255\
\029\001\030\001\031\001\032\001\002\001\255\255\255\255\255\255\
\255\255\255\255\008\001\009\001\255\255\255\255\255\255\255\255\
\014\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\255\255\056\001\255\255\004\001\255\255\029\001\
\030\001\031\001\032\001\010\001\255\255\255\255\013\001\255\255\
\015\001\255\255\017\001\255\255\019\001\255\255\255\255\255\255\
\023\001\024\001\004\001\255\255\255\255\028\001\029\001\053\001\
\010\001\255\255\056\001\013\001\255\255\015\001\255\255\017\001\
\255\255\019\001\255\255\255\255\255\255\023\001\255\255\004\001\
\255\255\255\255\028\001\029\001\255\255\010\001\053\001\255\255\
\013\001\056\001\015\001\255\255\017\001\004\001\019\001\255\255\
\255\255\255\255\023\001\010\001\255\255\255\255\013\001\028\001\
\015\001\255\255\017\001\053\001\255\255\255\255\056\001\255\255\
\023\001\255\255\255\255\255\255\255\255\028\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\255\255\255\255\056\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\255\255\
\255\255\056\001"

let yynames_const = "\
  "

let yynames_block = "\
  TYPE\000\
  INERT\000\
  LAMBDA\000\
  TTOP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  BOOL\000\
  FIX\000\
  LETREC\000\
  USTRING\000\
  UNIT\000\
  UUNIT\000\
  TIMESFLOAT\000\
  UFLOAT\000\
  LEQ\000\
  ALL\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  NAT\000\
  SOME\000\
  LET\000\
  IN\000\
  AS\000\
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
# 126 "parser.mly"
      ( fun ctx -> [],ctx )
# 560 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 128 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 572 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 136 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 579 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 138 "parser.mly"
      ( fun ctx -> ((Bind(_1.i, _1.v, _2 ctx)), addname ctx _1.v) )
# 587 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 140 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 595 "parser.ml"
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
# 142 "parser.mly"
     ( fun ctx ->
         let ctx1 = addname ctx _2.v in
         let ctx2 = addname ctx1 _4.v in
         (SomeBind(_1,_2.v,_4.v,_7 ctx), ctx2) )
# 611 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 150 "parser.mly"
      ( fun ctx -> VarBind (_2 ctx))
# 619 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 152 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 627 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 157 "parser.mly"
                ( _1 )
# 634 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'OType) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 159 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TyAll(_2.v,_3 ctx,_5 ctx1) )
# 647 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 166 "parser.mly"
           ( _2 )
# 656 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 168 "parser.mly"
      ( fun ctx ->
          if isnamebound ctx _1.v then
            TyVar(name2index _1.i ctx _1.v, ctxlength ctx)
          else 
            TyId(_1.v) )
# 667 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 174 "parser.mly"
      ( fun ctx -> TyTop )
# 674 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 176 "parser.mly"
      ( fun ctx -> TyBool )
# 681 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'FieldTypes) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 178 "parser.mly"
      ( fun ctx ->
          TyRecord(_2 ctx 1) )
# 691 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 181 "parser.mly"
      ( fun ctx -> TyString )
# 698 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 183 "parser.mly"
      ( fun ctx -> TyUnit )
# 705 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 185 "parser.mly"
      ( fun ctx -> TyFloat )
# 712 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 187 "parser.mly"
      ( fun ctx -> TyNat )
# 719 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'OType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 189 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _3.v in
          TySome(_3.v, _4 ctx, _6 ctx1) )
# 734 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    Obj.repr(
# 195 "parser.mly"
      ( fun ctx -> TyVarBind(TyTop) )
# 740 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 197 "parser.mly"
      ( fun ctx -> TyVarBind(_2 ctx) )
# 748 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 199 "parser.mly"
      ( fun ctx -> TyAbbBind(_2 ctx) )
# 756 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 205 "parser.mly"
     ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 765 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 207 "parser.mly"
            ( _1 )
# 772 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 211 "parser.mly"
      ( _1 )
# 779 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 213 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 793 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 217 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
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
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 223 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 831 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 225 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 843 "parser.ml"
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
# 227 "parser.mly"
      ( fun ctx -> 
          let ctx1 = addname ctx _2.v in 
          TmLet(_1, _2.v, TmFix(_1, TmAbs(_1, _2.v, _4 ctx, _6 ctx1)),
                _8 ctx1) )
# 860 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'OType) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 232 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmTAbs(_1,_2.v,_3 ctx,_5 ctx1) )
# 873 "parser.ml"
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
# 236 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _3.v in
          let ctx2 = addname ctx1 _5.v in
          TmUnpack(_1,_3.v,_5.v,_8 ctx,_10 ctx2) )
# 892 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 243 "parser.mly"
      ( _1 )
# 899 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 245 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 910 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 250 "parser.mly"
      ( fun ctx ->
          TmFix(_1, _2 ctx) )
# 919 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 253 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 928 "parser.ml"
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
# 941 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 260 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 949 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 262 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 957 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 264 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 965 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 268 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, _3.v) )
# 975 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 271 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 985 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AscribeTerm) in
    Obj.repr(
# 274 "parser.mly"
      ( _1 )
# 992 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 278 "parser.mly"
      ( fun ctx i -> [] )
# 998 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 280 "parser.mly"
      ( _1 )
# 1005 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FieldType) in
    Obj.repr(
# 284 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 1012 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'FieldType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 286 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 1021 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 290 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 1030 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 292 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 1037 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ATerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 296 "parser.mly"
      ( fun ctx -> TmAscribe(_2, _1 ctx, _3 ctx) )
# 1046 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 298 "parser.mly"
      ( _1 )
# 1053 "parser.ml"
               : 'AscribeTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 302 "parser.mly"
      ( _1 )
# 1060 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TermSeq) in
    Obj.repr(
# 304 "parser.mly"
      ( fun ctx ->
          TmApp(_2, TmAbs(_2, "_", TyUnit, _3 (addname ctx "_")), _1 ctx) )
# 1070 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TermSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 310 "parser.mly"
      ( _2 )
# 1079 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 312 "parser.mly"
      ( fun ctx -> TmInert(_1, _3 ctx) )
# 1089 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 314 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 1097 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 317 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 1104 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 319 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 1111 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 321 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 1121 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 324 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 1128 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 326 "parser.mly"
      ( fun ctx -> TmUnit(_1) )
# 1135 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 328 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 1142 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 330 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 1153 "parser.ml"
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
# 336 "parser.mly"
      ( fun ctx ->
          TmPack(_1,_3 ctx,_5 ctx,_8 ctx) )
# 1168 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 341 "parser.mly"
      ( fun ctx i -> [] )
# 1174 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 343 "parser.mly"
      ( _1 )
# 1181 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 347 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 1188 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 349 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 1197 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 353 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 1206 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 355 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 1213 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    Obj.repr(
# 359 "parser.mly"
      ( fun ctx -> TyTop)
# 1219 "parser.ml"
               : 'OType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 361 "parser.mly"
      ( _2 )
# 1227 "parser.ml"
               : 'OType))
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
