open Syntax
module Env = Map.Make (String)

type value = VInt of int64 | VBool of bool
and env = value Env.t

let string_of_value v =
  match v with VInt n -> Int64.to_string n | VBool b -> string_of_bool b

let err_unbound_var = "Error: Unbound variable"
let err_type_mismatch = "Error: Type mismatch"

let rec eval (env : env) (e : 'a expr) : value =
  match e with
  | ENumber (n, _) -> VInt n
  | EPrim1 (op, e, _) -> eval_prim1 env op e
  | EPrim2 (op, l, r, _) -> eval_prim2 env op l r
  | EId (x, _) -> eval_id env x
  | ELet (x, e1, e2, _) -> eval_let env x e1 e2
  | EIf (cond, thn, els, _) -> eval_if env cond thn els

and eval_id env x =
  match Env.find_opt x env with Some v -> v | None -> failwith err_unbound_var

and eval_let env x e1 e2 =
  let v = eval env e1 in
  let env' = Env.add x v env in
  eval env' e2

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool b -> if b then eval env e2 else eval env e3
  | VInt _ -> failwith err_type_mismatch

and eval_prim1 env op e =
  match (op, eval env e) with
  | Neg, VInt n -> VInt (Int64.neg n)
  | _ -> failwith err_type_mismatch

and eval_prim2 env op l r =
  match (op, eval env l, eval env r) with
  | Add, VInt a, VInt b -> VInt (Int64.add a b)
  | Sub, VInt a, VInt b -> VInt (Int64.sub a b)
  | Mul, VInt a, VInt b -> VInt (Int64.mul a b)
  | _ -> failwith err_type_mismatch
