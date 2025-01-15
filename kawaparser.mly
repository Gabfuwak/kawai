%{

  open Lexing
  open Kawa

%}

%token INT_TYPE BOOL_TYPE VOID CLASS THIS NEW EXTENDS
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
%left DOT

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
| vlist=list(var_decl) classes=list(class_def) MAIN BEGIN main=list(instr) END EOF
    { {classes=classes; globals=(List.flatten vlist); main} } (*flatten pour gerer les definitions en serie*)
;


typ:
| b=BOOL_TYPE {TBool}
| n=INT_TYPE {TInt}
| v=VOID {TVoid}
| id=IDENT {TClass id}
;

(* Definition de variable, attribut ou methode. Tout ce qui a un type quoi. On met ça la pour eviter les conflits avec les declarations simplifiées *)
typed_def: 
| t=typ id_list=separated_nonempty_list(COMMA, IDENT) SEMI 
    { VarAttr_list (List.map (fun id -> (id, t)) id_list) }
| t=typ id=IDENT LPAR params=separated_list(COMMA, method_param) RPAR 
  BEGIN body=list(method_line) END
    {
      (* body est maintenant une liste de listes qu'il faut aplatir *)
      let flat_body = List.flatten body in
      let (variables, code) = List.partition(
        function MemberVar _ -> true | MemberInstr _ -> false
      ) flat_body in

      let variables = List.map (function 
        | MemberVar v -> v 
        | _ -> failwith "unreachable") variables in
      let code = List.map (function 
        | MemberInstr i -> i 
        | _ -> failwith "unreachable") code in

      Meth {  (* Enlever les crochets ici *)
        method_name = id;
        code = code;
        params = params;
        locals = variables;
        return = t;
      }
    }
;

(*meme idée que class_memeber, sauf que au lieu d'avoir des attr et des method_def, on a des variables locales et des instructions*)
method_line:
| t=typ id_list=separated_nonempty_list(COMMA, IDENT) SEMI 
    { List.map (fun id -> MemberVar (id, t)) id_list }
| i=instr { [MemberInstr i] }
;

var_decl:
| d=typed_def { 
    match d with 
    | VarAttr_list vars -> vars
    | _ -> failwith "unreachable" 
  } 
;

class_def:
| CLASS name=IDENT parent=option(extends_clause) BEGIN members=list(typed_def) END 
    { 
      let (attributes, methods) = List.partition (
        function VarAttr_list _ -> true | Meth _ -> false
      ) members in
      
      let attributes = List.flatten (List.map (function 
        | VarAttr_list vars -> vars 
        | _ -> failwith "unreachable") attributes) in
      let methods = List.map (function 
        | Meth m -> m 
        | _ -> failwith "unreachable") methods in

      {
        class_name = name;
        attributes = attributes;
        methods = methods;
        parent = parent;
      }
    }
;

extends_clause:
| EXTENDS parent_name=IDENT {parent_name}
;



(*returns (string, typ)*)
method_param:
| t=typ id=IDENT { (id, t) } 
;



instr:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| id=mem_access SET e=expression SEMI { Set(id, e) }
| WHILE LPAR e=expression RPAR BEGIN instrlist=list(instr) END { While(e, instrlist) }
| IF LPAR e=expression RPAR BEGIN instrlist=list(instr) END elseblock=option(else_block) 
  {
    match elseblock with
    | None ->    If(e, instrlist, [])
    | Some block ->  If(e, instrlist, block) 
  }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
;


else_block: (*renvoie une liste d'instructions*)
| ELSE BEGIN instrlist=list(instr) END { instrlist }
| ELSE IF LPAR e=expression RPAR BEGIN instrlist=list(instr) END elseblock=option(else_block)
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
