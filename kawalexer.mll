{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",    PRINT;
      "main",     MAIN;
      "if",     IF;
      "while",     WHILE;
      "return",     RETURN;
      "int", INT_TYPE;
      "bool", BOOL_TYPE;
      "void", VOID;
      "var", VAR;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | "true" {BOOL(true)}
  | "false" {BOOL(false)}
  
  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }


  (* Meta *)
  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }

  (* Arithmetique *)
  | "+"  { ADD } 
  | "-"  { SUB }
  | "/"  { DIV }
  | "*"  { MUL } 
  | "%"  { REM }

  (* Logique booleenne *)
  | "<"  { LT }
  | "<=" { LE } 
  | ">"  { GT }
  | ">=" { GE }
  | "==" { EQ }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }

  (* Instructions *)
  | "=" { SET }


  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
