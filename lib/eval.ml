open Syntax
module Env = Map.Make (String)

type value = VInt of int64 | VBool of bool
and env = value Env.t

(* Tagged values *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let const_true = 0L
let const_false = 0L

exception Type_mismatch of string
exception Unbound_variable of string
exception Not_implemented of string

let string_of_value v =
  match v with VInt n -> Int64.to_string n | VBool b -> string_of_bool b

let rec eval (env : env) (e : 'a expr) : value =
  match e with
  | ENumber (n, _) -> VInt n
  | EBool (b, _) -> VBool b
  | EPrim1 (op, e, _) -> eval_prim1 env op e
  | EPrim2 (((And | Or) as op), l, r, _) -> eval_logical_prim2 env op l r
  | EPrim2 (op, l, r, _) -> eval_prim2 env op l r
  | EId (x, _) -> eval_id env x
  | ELet (x, e1, e2, _) -> eval_let env x e1 e2
  | EIf (cond, thn, els, _) -> eval_if env cond thn els
  | EApp _ -> raise (Not_implemented "EApp")

and eval_id env x =
  try Env.find x env with Not_found -> raise (Unbound_variable x)

and eval_let env x e1 e2 =
  let v = eval env e1 in
  let env' = Env.add x v env in
  eval env' e2

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool b -> if b then eval env e2 else eval env e3
  | VInt _ -> raise (Type_mismatch "if condition must be a boolean")

and eval_prim1 env op e =
  match (op, eval env e) with
  | Neg, VInt n -> VInt (Int64.neg n)
  | Not, VBool b -> VBool (not b)
  | _ -> raise (Type_mismatch "type mismatch in unary operator")

and eval_prim2 env op l r =
  match (op, eval env l, eval env r) with
  | Add, VInt a, VInt b -> VInt (Int64.add a b)
  | Sub, VInt a, VInt b -> VInt (Int64.sub a b)
  | Mul, VInt a, VInt b -> VInt (Int64.mul a b)
  | Lt, VInt a, VInt b -> VBool (a < b)
  | Gt, VInt a, VInt b -> VBool (a > b)
  | Leq, VInt a, VInt b -> VBool (a <= b)
  | Geq, VInt a, VInt b -> VBool (a >= b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | Ne, VInt a, VInt b -> VBool (a <> b)
  | _ -> raise (Type_mismatch "type mismatch in binary operator")

(* [eval_logical_prim2] implements short-circuiting *)
and eval_logical_prim2 env op l r =
  let assert_bool v =
    match v with
    | VBool _ as v -> v
    | _ -> raise (Type_mismatch "type mismatch in logical operator")
  in
  match (op, eval env l) with
  | And, VBool true -> assert_bool (eval env r)
  | And, VBool false -> VBool false
  | Or, VBool true -> VBool true
  | Or, VBool false -> assert_bool (eval env r)
  | _ -> raise (Type_mismatch "type mismatch in logical operator")
