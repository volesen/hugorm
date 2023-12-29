%{
  open Syntax
%}

%token <int64> INT
%token <string> ID

%token TRUE
%token FALSE

%token IF
%token THEN
%token ELSE

%token LET
%token IN

%token DEF

%token PLUS
%token MINUS
%token STAR
%token EQ
%token NE
%token LT
%token LEQ
%token GT
%token GEQ

%token COMMA

%token FST
%token SND
%token NIL

%token LEFT_PAREN
%token RIGHT_PAREN

%token EOF

%nonassoc IN
%nonassoc ELSE
%right EQ NE
%left LT LEQ GT GEQ
%left PLUS MINUS 
%left STAR
%left FST SND

%start program

%type <unit program> program
%type <unit decl> decl
%type <string list> params
%type <unit expr> expr
%type <unit expr list> args
%type <prim1> prim1
%type <prim2> prim2

%%

program:
  | decls=list(decl); body=expr; EOF { { decls; body } }

decl:
  | DEF; f=ID; params=params; body=expr; IN { DFun(f, params, body, ()) }

params:
  | LEFT_PAREN; params=separated_list(COMMA, ID); RIGHT_PAREN { params }

expr:
  | NIL; { ENil () }
  | LEFT_PAREN; e=expr; RIGHT_PAREN { e }
  | i=INT { ENumber(i, ()) }
  | TRUE { EBool(true, ()) }
  | FALSE { EBool(false, ()) }
  | x=ID { EId(x, ()) }
  | LET; x=ID; EQ; b=expr; IN; e=expr { ELet(x, b, e, ()) }
  | IF; cond=expr; THEN; thn=expr; ELSE; els=expr { EIf(cond, thn, els, ()) }
  | op=prim1; e=expr; { EPrim1(op, e, ()) }
  | l=expr; op=prim2; r=expr { EPrim2(op, l, r, ()) }
  | f=ID; args=args { EApp(f, args, ()) }
  | LEFT_PAREN; fst=expr; COMMA; snd=expr; RIGHT_PAREN { EPair(fst, snd, ()) }

args: 
  | LEFT_PAREN; args=separated_list(COMMA, expr); RIGHT_PAREN { args }

%inline prim1:
  | MINUS { Neg }
  | FST { Fst }
  | SND { Snd }

%inline prim2:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | EQ { Eq }
  | NE { Ne }
  | LT { Lt }
  | LEQ { Leq }
  | GT { Gt }
  | GEQ { Geq }
