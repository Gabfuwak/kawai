%{

  open Lexing
  open Kawa

%}

%token INT_TYPE BOOL_TYPE VOID VAR CLASS ATTRIBUTE METHOD THIS NEW
%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT
%token ADD SUB MUL DIV REM
%token LT LE GT GE EQ NEQ AND OR NOT
%token PRINT
%token EOF

%token IF ELSE WHILE RETURN SET

%token USUB (* moins unaire, n'existe que pour le %nonassoc *)

(* Precedence *)
%right NOT
%left AND OR
%left EQ NEQ
%left LT LE GT GE
%left ADD SUB
%left DIV REM
%left MUL

%nonassoc USUB (* Pour regler le conflit entre le moins binaire et unaire *)

%start program
%type <Kawa.program> program

%%

program:
| vlist=list(var_decl) classes=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes=classes; globals=vlist; main} }
;


typ:
| b=BOOL_TYPE {TBool}
| n=INT_TYPE {TInt}
| v=VOID {TVoid}
| id=IDENT {TClass id}
;

var_decl:
| VAR t=typ id=IDENT SEMI { (id, t) }
;

class_def:
| CLASS name=IDENT (*TODO: option(extends_clause)*) BEGIN attributes=list(attr_decl) (*methods=list(method_def)*)  END 
    { 
      {
        class_name = name;
        attributes = attributes;
        methods = [](*methods*);
        parent = None; (*todo: inheritance*)
      }
    }
;

(*
TODO: inheritance
extends_clause:
| EXTENDS parent_name=IDENT {parent_name}
;
*)

attr_decl:
| ATTRIBUTE t=typ id=IDENT SEMI { (id, t) }
;

method_def:
| METHOD t=typ id=IDENT LPAR params=separated_list(COMMA, method_param) RPAR BEGIN variables=list(var_decl) code=list(instr) END
    {
      {
        method_name = id;
        code = code;
        params = params;
        locals = variables;
        return = t;
      }
    }
;

(*TODO, return (string, typ)*)
method_param:
| t=typ id=IDENT { (id, t) } 
;



instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| id=mem_access SET e=expression SEMI { Set(id, e) }
| WHILE LPAR e=expression RPAR BEGIN instrlist=list(instruction) END { While(e, instrlist) }
| IF LPAR e=expression RPAR BEGIN instrlist=list(instruction) END elseblock=option(else_block) 
  {
    match elseblock with
    | None ->    If(e, instrlist, [])
    | Some block ->  If(e, instrlist, block) 
  }
| RETURN e=expression SEMI { Return(e) }
| expression SEMI { Expr(e) }
;


else_block: (*renvoie une liste d'instructions*)
| ELSE BEGIN instrlist=list(instruction) END { instrlist }
| ELSE IF LPAR e=expression RPAR BEGIN instrlist=list(instruction) END elseblock=option(else_block)
  { match elseblock with
    | None ->    [If(e, instrlist, [])]
    | Some block ->  [If(e, instrlist, block)] }
;

mem_access:
| id=IDENT { Var(id) }
| e=expression DOT id=IDENT { Field(e, id) }
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
| SUB e=expression %prec USUB { Unop(Opp, e)}

(* Logique booleene *)
| e1=expression LT  e2=expression {Binop(Lt, e1, e2)}
| e1=expression LE  e2=expression {Binop(Le, e1, e2)}
| e1=expression GT  e2=expression {Binop(Gt, e1, e2)}
| e1=expression GE  e2=expression {Binop(Ge, e1, e2)}
| e1=expression EQ  e2=expression {Binop(Eq, e1, e2)}
| e1=expression NEQ e2=expression {Binop(Neq, e1, e2)}
| e1=expression AND e2=expression {Binop(And, e1, e2)}
| e1=expression OR  e2=expression {Binop(Or, e1, e2)}
| NOT e=expression {Unop(Not, e)}

(* Variables *)
| m=mem_access {Get(m)}

(* Classes *)
| NEW id=IDENT { New(id) }
| NEW id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { NewCstr(id, args) } 
| e=expression DOT id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { MethCall(e, id, args) }
| THIS { This }
;
