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

let rec is_subclass subcls supercls cenv =
  if supercls = subcls then true
  else
    let subclass = Env.find subcls cenv in
    match subclass.parent with
    | None -> false
    | Some subcls_parent -> is_subclass subcls_parent supercls cenv         


let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let cenv = add_classes p.classes Env.empty in

  let rec check e typ tenv cenv = 
    let typ_e = type_expr e tenv cenv in
    match typ_e, typ with
    | TClass t1, TClass t2 -> if not (is_subclass t1 t2 cenv) then type_error typ_e typ;
    | _ -> if typ_e <> typ then type_error typ_e typ;

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
        | _ -> failwith "Binop implementation missing (should be unreachable)";
    )
    | New(class_name) -> 
        try ( 
          let _ = Env.find cenv class_name in
          TClass class_name
        )
        with Not_found -> error("Class \"" ^ class_name ^ "\" does not exist." );
    | NewCstr(class_name, args) -> (
        try(
          let class_definition = Env.find cenv class_name in
          try (
            let method = List.find (fun method_definition -> 
              method_definition.method_name = "constructor") class_definition.methods in
            
            try (
              if method.return <> TVoid then
                error("Constructor of class \"" ^ class_name ^ "\" must return void");
              
              List.iter2 (fun arg (_, arg_exp_type) -> 
                check arg arg_exp_type tenv cenv
              ) args method.params;
              
                
              TClass class_name 
            ) with Invalid_argument _ -> 
              error("Constructor of class \"" ^ class_name ^ "\" expects " ^ 
                    string_of_int (List.length method.params) ^ " arguments but got " ^ 
                    string_of_int (List.length args))

          ) with Not_found -> 
            error("No constructor method defined for class \"" ^ class_definition.class_name ^ "\".")
        ) with Not_found -> 
          error("Class \"" ^ class_name ^ "\" does not exist.")
    )
    | MethCall(obj_expr, method_name, args) -> (
        let rec find_method class_name =
            let cls = try 
                Env.find cenv class_name 
            with Not_found -> 
                error("Undefined class: " ^ class_name) in
            
            try 
                let method_def = List.find 
                    (fun m -> m.method_name = method_name) 
                    cls.methods in
                
                (* Check arguments match parameter types *)
                try
                    List.iter2 
                        (fun arg (_, arg_exp_type) -> check arg arg_exp_type tenv cenv) 
                        args method_def.params;
                    method_def.return
                with Invalid_argument _ ->
                    error("Method '" ^ method_name ^ "' expects " ^ 
                         string_of_int (List.length method_def.params) ^ 
                         " arguments but got " ^ string_of_int (List.length args))
                         
            with Not_found -> 
                match cls.parent with
                | None -> error("Method '" ^ method_name ^ "' not found in class " ^ class_name)
                | Some parent_name -> find_method parent_name
        in
        
        match type_expr obj_expr tenv cenv with
        | TClass class_name -> find_method class_name
        | _ -> error("Cannot call method on non-object type")
    )
    | _ -> failwith "case not implemented in type_expr (should be unreachable)"

  and type_mem_access m tenv cenv = match m with
    | Var x -> (try Env.find tenv x
                with Not_found -> error("Undefined variable \"" ^ x ^ "\"." ))
    | Field(obj_expr, field_name) ->(
        let rec find_field class_name =  
            let cls = try 
                Env.find cenv class_name 
            with Not_found -> 
                error("Undefined class: " ^ class_name) in
            
            try 
                snd (List.find (fun elem -> fst elem = field_name) cls.attributes)
            with Not_found -> 
                match cls.parent with
                | None -> error("Field '" ^ field_name ^ "' not found in class " ^ class_name)
                | Some parent_name -> find_field parent_name
        in        
        match type_expr obj_expr tenv cenv with
        | TClass class_name -> find_field class_name
        | _ -> error("Cannot access field of non-object type");
    )
        
        
    | _ -> failwith "case not implemented in type_mem_access (should be unreachable)"
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
    | Return e -> 
        if ret = TVoid then
            error "Return statement in void method"
        else
            check e ret tenv cenv
    | Expr e -> 
        let _ = type_expr e tenv cenv in
        ()
    | _ -> failwith "case not implemented in check_instr (should be unreachable)"
  and check_seq s ret tenv cenv =
    List.iter (fun i -> check_instr i ret tenv cenv) s
  in

let rec check_method class_name meth tenv cenv =
  let tenv = add_env meth.params tenv in
  let tenv = add_env meth.locals tenv in
  let tenv = Env.add "this" (TClass class_name) tenv in
  check_seq meth.code meth.return tenv cenv

and check_class cls cenv =
  ( (*check parent existance*)
  match cls.parent with
    | Some parent_name ->
        if not (Env.mem parent_name cenv) then
          error ("Unknown parent class \"" ^ parent_name ^ "\" for class \"" ^ cls.class_name ^ "\"")
    | None -> ()
  );

  let tenv = add_env cls.attributes Env.empty in

  List.iter (fun (_, typ) ->
    match typ with
    | TClass class_name -> 
        try Env.find class_name cenv with Not_found -> error("Unknown class \"" ^ class_name ^ "\".")
    | _ -> ()
  ) cls.attributes
  
  List.iter (fun meth -> check_method cls.class_name meth tenv cenv) cls.methods
in

List.iter (fun cls -> check_class cls cenv) p.classes;
check_seq p.main TVoid tenv cenv
