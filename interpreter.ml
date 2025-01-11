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

module Env = Map.Make(String)
type cenv = class_def Env.t

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  let cenv = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  List.iter (fun cls_def -> Hashtbl.add cenv cls_def.class_name cls_def) p.classes;
  
  let rec eval_call f this args =
    let method_scope = Hashtbl.create 8 in
    Hashtbl.add method_scope "this" (VObj this);
    List.iter2 (fun arg_val (arg_name, _) -> Hashtbl.add method_scope arg_name arg_val;) args f.params;
    List.iter (fun (lvar, _) -> Hashtbl.add method_scope lvar Null) f.locals;
    try 
      exec_seq f.code method_scope;
      Null
    with Return v -> v

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
      | This -> ( 
          try Hashtbl.find lenv "this"
          with Not_found -> raise (Error "\"this\" called outside of class context");
      )
      | New class_name ->
          let cls = try Hashtbl.find cenv class_name
          with Not_found -> raise (Error ("Unknown class: \"" ^ class_name ^ "\""));
          in
          let fields = Hashtbl.create 16 in
          List.iter (fun (attr_name, _) -> Hashtbl.add fields attr_name Null) cls.attributes;
          VObj {cls=class_name; fields=fields}
      | NewCstr(class_name, args) -> (
          let cls = try Hashtbl.find cenv class_name
          with Not_found -> raise (Error ("Unknown class: \"" ^ class_name ^ "\""));
          in

          let fields = Hashtbl.create 16 in
          let evaluated_args = List.map (fun arg -> eval arg lenv) args in

          List.iter (fun (attr_name, _) -> Hashtbl.add fields attr_name Null;) cls.attributes;
          
          let ret_obj = {cls=class_name; fields=fields} in
           
          let _ = eval_call (List.find (fun meth -> meth.method_name = "constructor") cls.methods) ret_obj evaluated_args in

          VObj ret_obj


      )
      | Get(Field(e, f)) ->(
          let obj = match eval e lenv with
            | VObj o -> o
            | _ -> raise (Error "Field access on non-object")
          in
          (try Hashtbl.find obj.fields f
           with Not_found -> raise (Error ("unknown field " ^ f))
           )
        )
      | MethCall(obj_expr, method_name, args) -> (
          match eval obj_expr lenv with
          | VObj obj -> (
              let class_def = Hashtbl.find cenv obj.cls in
              let method_def = List.find 
              (fun m -> m.method_name = method_name) 
              class_def.methods in
              let evaluated_args = List.map (fun arg -> eval arg lenv) args in
              eval_call method_def obj evaluated_args
          )
          | _ -> raise (Error "Method call on non-object value")
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
      | Set(Field(e, f), e2) ->
          let obj = match eval e lenv with
            | VObj o -> o 
            | _ -> raise (Error "field assignment to non-object")
          in
          let v = eval e2 lenv in
          Hashtbl.replace obj.fields f v
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
      | While(cond, block) ->(
          while (evalb cond lenv) do
            exec_seq block lenv;
          done
        )
      | Expr e -> let _ = eval e lenv in ()
      | Return e ->
          raise (Return (eval e lenv))

          
      | _ -> failwith "case not implemented in exec"
    and exec_seq s lenv = 
      List.iter (fun i -> exec i lenv) s
    in

    exec_seq s lenv
  in
  
  exec_seq p.main (Hashtbl.create 1)
