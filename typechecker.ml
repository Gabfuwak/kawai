open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Get(m) -> type_mem_access m tenv
    | Unop(op, e) ->(
      let type_e = type_expr e tenv in
      match op with
      | Opp -> 
          if type_e <> TInt then type_error type_e TInt;
          TInt
      | Not ->
          if type_e <> TBool then type_error type_e TBool;
          TBool
    )
    | Binop(op, e1, e2) ->(
        let type_e1 = type_expr e1 tenv in
        let type_e2 = type_expr e2 tenv in
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
    | _ -> failwith "case not implemented in type_expr"

  and type_mem_access m tenv = match m with
    | Var x -> (try Env.find tenv x
                with Not_found -> error("Undefined variable"))
    | _ -> failwith "case not implemented in type_mem_access"
  in

  let rec check_instr i ret tenv = match i with
    | Print e ->
      let type_e = type_expr e tenv in
      (match type_e with
      | TInt | TBool -> ()
      | _ -> type_error type_e TInt; (*TODO: Modifier ça pour accepter uniquement des string, pour l'instant c'est du debug et print est très minimal*)
      )
    | Set(m, e) ->
        let typ_m = type_mem_access m tenv in
        check e typ_m tenv
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  check_seq p.main TVoid tenv
