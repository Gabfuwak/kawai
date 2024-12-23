%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token ADD SUB MUL DIV REM
%token LT LE GT GE EQ NEQ AND OR
%token PRINT
%token EOF

%token IF WHILE RETURN SET

(* Precedence *)
%left LT LE GT GE EQ NEQ AND OR
%left ADD SUB
%left DIV REM
%left MUL

%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
(* Terminaux *)
| n=INT  { Int(n) }
| b=BOOL { Bool(b) }

(* Support parentheses *)
| LPAR e=expression RPAR { e }

(* Arithmetique *)
| e1=expression ADD e2=expression {Binop(Add, e1, e2)}
| e1=expression SUB e2=expression {Binop(Sub, e1, e2)}
| e1=expression MUL e2=expression {Binop(Mul, e1, e2)}
| e1=expression DIV e2=expression {Binop(Div, e1, e2)}
| e1=expression REM e2=expression {Binop(Rem, e1, e2)}

(* Logique booleene *)
| e1=expression LT  e2=expression {Binop(Lt, e1, e2)}
| e1=expression LE  e2=expression {Binop(Le, e1, e2)}
| e1=expression GT  e2=expression {Binop(Gt, e1, e2)}
| e1=expression GE  e2=expression {Binop(Ge, e1, e2)}
| e1=expression EQ  e2=expression {Binop(Eq, e1, e2)}
| e1=expression NEQ e2=expression {Binop(Neq, e1, e2)}
| e1=expression AND e2=expression {Binop(And, e1, e2)}
| e1=expression OR  e2=expression {Binop(Or, e1, e2)}

;
