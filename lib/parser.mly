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

%token PLUS
%token MINUS
%token STAR
%token EQ
%token LT
%token LTE
%token GT
%token GTE

%token LEFT_PAREN
%token RIGHT_PAREN

%token EOF

%nonassoc IN
%nonassoc ELSE
%right EQ
%left LT LTE GT GTE
%left PLUS MINUS 
%left STAR

%start program

%type <unit program> program
%type <unit expr> expr
%type <prim1> prim1
%type <prim2> prim2

%%

program:
  | e = expr; EOF { e }

expr:
  | LEFT_PAREN; e=expr; RIGHT_PAREN { e }
  | i=INT { ENumber(i, ()) }
  | TRUE { EBool(true, ()) }
  | FALSE { EBool(false, ()) }
  | x=ID { EId(x, ()) }
  | LET; x=ID; EQ; b=expr; IN; e=expr { ELet(x, b, e, ()) }
  | IF; cond=expr; THEN; thn=expr; ELSE; els=expr { EIf(cond, thn, els, ()) }
  | op=prim1; e=expr; { EPrim1(op, e, ()) }
  | l=expr; op=prim2; r=expr { EPrim2(op, l, r, ()) }

%inline prim1:
  | MINUS { Neg }

%inline prim2:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | EQ { Eq }
  | LT { Less }
  | LTE { LessEq }
  | GT { Greater }
  | GTE { GreaterEq }
