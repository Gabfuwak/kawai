open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t
type cenv = class_def Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let add_classes classes cenv = 
  List.fold_left (fun env c -> Env.add c.class_name c env) cenv classes

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let cenv = add_classes p.classes Env.empty in

  let rec check e typ tenv cenv = 
    let typ_e = type_expr e tenv cenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv cenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Get(m) -> type_mem_access m tenv cenv
    | Unop(op, e) ->(
      let type_e = type_expr e tenv cenv in
      match op with
      | Opp -> 
          if type_e <> TInt then type_error type_e TInt;
          TInt
      | Not ->
          if type_e <> TBool then type_error type_e TBool;
          TBool
    )
    | Binop(op, e1, e2) ->(
        let type_e1 = type_expr e1 tenv cenv in
        let type_e2 = type_expr e2 tenv cenv in
        match op with
        | Add | Sub | Mul | Div | Rem ->
            if type_e1 <> TInt then type_error type_e1 TInt;
            if type_e2 <> TInt then type_error type_e2 TInt;
            TInt

        | Lt | Le | Gt | Ge ->
            if type_e1 <> TInt then type_error type_e1 TInt;
            if type_e2 <> TInt then type_error type_e2 TInt;
            TBool

        | Eq | Neq ->
            if type_e1 <> type_e2 then type_error type_e2 type_e1;
            TBool

        | And | Or ->
            if type_e1 <> TBool then type_error type_e1 TBool;
            if type_e2 <> TBool then type_error type_e2 TBool;
            TBool
        | _ -> failwith "To be implemented";
    )
    | New(class_name) -> 
        try 
          let _ = Env.find cenv class_name in
          TClass class_name
        with Not_found -> error("Class \"" ^ class_name ^ "\" does not exist." );
    | NewCstr(class_name, _) -> error("TODO: implement constructors");
    | _ -> failwith "case not implemented in type_expr"

  and type_mem_access m tenv cenv = match m with
    | Var x -> (try Env.find tenv x
                with Not_found -> error("Undefined variable"))
    | Field(e, id) ->(
        match type_expr e tenv cenv with
        | TClass class_name ->  
            let cls = try Env.find cenv class_name with Not_found -> error("Undefined class:" ^ class_name);
            try snd (List.find (fun elem -> fst elem = id) cls.attributes) with Not_found -> error("Undefined attribute \"" ^ id ^ "\" for class " ^ class_name);
        | _ -> error("Cannot access field of non-object type");
        )
        )
        
    | _ -> failwith "case not implemented in type_mem_access"
  in

  let rec check_instr i ret tenv cenv = match i with
    | Print e ->
      let type_e = type_expr e tenv cenv in
      (match type_e with
      | TInt | TBool -> ()
      | _ -> type_error type_e TInt; (*TODO: Modifier ça pour accepter uniquement des string, pour l'instant c'est du debug et print est très minimal*)
      )
    | Set(m, e) ->
        let typ_m = type_mem_access m tenv cenv in
        check e typ_m tenv cenv;
    | If(cond, blockif, blockelse) ->(
        check cond TBool tenv cenv;
        check_seq blockif ret tenv cenv;
        check_seq blockelse ret tenv cenv;
    )
    | While(cond, block) ->(
        check cond TBool tenv cenv;
        check_seq block ret tenv cenv;
    )
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv cenv =
    List.iter (fun i -> check_instr i ret tenv cenv) s
  in

  check_seq p.main TVoid tenv cenv
