open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    failwith "eval_call not implemented"

  and exec_seq s lenv =
    let rec evali e lenv = match eval e lenv with
      | VInt n -> n
      | _ -> assert false
    and evalb e lenv = match eval e lenv with
      | VBool b -> b
      | _ -> assert false
    and evalo e lenv = match eval e lenv with
      | VObj o -> o
      | _ -> assert false
        
    and eval (e: expr) (lenv: (string, value) Hashtbl.t): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Get(Var x) ->( 
          try Hashtbl.find lenv x
          with Not_found ->
            try Hashtbl.find env x
            with Not_found ->
              raise (Error ("undefined variable: " ^ x))
      )
      | Unop (op, e) ->(
          let v = eval e lenv in
          match op, v with
          | Opp, VInt v -> VInt(-v)
          | Not, VBool v -> VBool(not v)
          | _ -> raise (Error "Unkown operation (unop eval) ");
      )
      | Binop (op, e1, e2) ->(
          let v1 = eval e1 lenv in
          let v2 = eval e2 lenv in
          match op, v1, v2 with
          | Add, VInt n1, VInt n2 -> VInt(n1 + n2)
          | Sub, VInt n1, VInt n2 -> VInt(n1 - n2)
          | Mul, VInt n1, VInt n2 -> VInt(n1 * n2)
          | Div, VInt n1, VInt n2 -> 
              if n2 = 0 then raise (Error "Divison by zero");
              VInt(n1 / n2)
          | Rem, VInt n1, VInt n2 -> 
              if n2 = 0 then raise (Error "Modulo by zero"); 
              VInt(n1 mod n2)
          
          | Lt, VInt n1, VInt n2 -> VBool(n1 < n2) 
          | Le, VInt n1, VInt n2 -> VBool(n1 <= n2) 
          | Gt, VInt n1, VInt n2 -> VBool(n1 > n2) 
          | Ge, VInt n1, VInt n2 -> VBool(n1 >= n2)
          
          | Eq, _, _ -> VBool(v1 = v2)
          | Neq, _, _ -> VBool(v1 <> v2)
          
          | And, VBool b1, VBool b2 -> VBool(b1 && b2)
          | Or, VBool b1, VBool b2 -> VBool(b1 || b2)

          | _ -> raise (Error "Unknown operation (binop eval)");
      )
      | _ -> raise (Error "Unknown operation (main eval)"); 
      
    in
  
    let rec exec (i: instr) (lenv: (string, value) Hashtbl.t): unit = match i with
      | Print e ->( 
          match eval e lenv with
          | VInt n -> Printf.printf "%d\n" n
          | VBool b -> Printf.printf "%b\n" b
          | Null -> Printf.printf "null"
          | _ -> Printf.printf "Type not printable yet!"
          )
      | Set(Var x, e) -> (
          let v = eval e lenv in 
          try Hashtbl.replace lenv x v
          with Not_found ->
            try Hashtbl.replace env x v 
            with Not_found ->
              raise (Error ("undefined variable " ^ x))
      )
      | If(cond, blockif, blockelse) ->(
        let c = match eval cond lenv with
        | VBool b -> b
        | _ -> failwith "unreachable: condition is not bool even after typechecking (things went real bad)"
        in
        if c then
          exec_seq blockif lenv
        else
          exec_seq blockelse lenv
      )
      | _ -> failwith "case not implemented in exec"
    and exec_seq s lenv = 
      List.iter (fun i -> exec i lenv) s
    in

    exec_seq s lenv
  in
  
  exec_seq p.main (Hashtbl.create 1)
