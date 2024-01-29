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
%token REC
%token IN

%token DEF

%token LAMBDA
%token ARROW

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

%token LEFT_CURLY
%token RIGHT_CURLY

%token LEFT_BRACKET
%token RIGHT_BRACKET

%token LEFT_PAREN
%token RIGHT_PAREN

%token EOF

%nonassoc ARROW
%nonassoc IN
%nonassoc ELSE
%right EQ NE
%left LT LEQ GT GEQ
%left PLUS MINUS 
%left STAR
%nonassoc UMINUS
%nonassoc LEFT_BRACKET
%nonassoc LEFT_PAREN

%start program

%type <Lexing.position program> program

%%

program:
  | decls=list(decl); body=expr; EOF { { decls; body } }

decl:
  | DEF; f=ID; params=params; body=expr; IN { DFun(f, params, body, $startpos) }

params:
  | LEFT_PAREN; params=separated_list(COMMA, ID); RIGHT_PAREN { params }

expr:
  | LEFT_PAREN; e=expr; RIGHT_PAREN { e }
  | i=INT { ENumber(i, $startpos) }
  | TRUE { EBool(true, $startpos) }
  | FALSE { EBool(false, $startpos) }
  | x=ID { EId(x, $startpos) }
  | LET; x=ID; EQ; b=expr; IN; e=expr { ELet(x, b, e, $startpos) }
  | IF; cond=expr; THEN; thn=expr; ELSE; els=expr { EIf(cond, thn, els, $startpos) }
  | op=prim1; e=expr %prec UMINUS { EPrim1(op, e, $startpos) }
  | l=expr; op=prim2; r=expr { EPrim2(op, l, r, $startpos) }
  | f=expr; args=args { EApp(f, args, $startpos) }
  | LEFT_CURLY; elements=separated_list(COMMA, expr); RIGHT_CURLY { ETuple(elements, $startpos)}
  | tuple=expr; LEFT_BRACKET; index=expr; RIGHT_BRACKET { EGetItem(tuple, index, $startpos) }
  | LAMBDA; params=params; ARROW; body=expr { ELambda(params, body, $startpos) }
  | LET; REC; x=ID; EQ; b=expr; IN; e=expr { ELetRec(x, b, e, $startpos) }

args: 
  | LEFT_PAREN; args=separated_list(COMMA, expr); RIGHT_PAREN { args }

%inline prim1:
  | MINUS { Neg }

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
