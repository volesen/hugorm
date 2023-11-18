open Syntax
module Env = Map.Make (String)

type value = VInt of int64
and env = value Env.t

let string_of_value v = match v with VInt n -> Int64.to_string n
let err_unbound_var = "Error: Unbound variable"

let rec eval (env : env) (e : expr) : value =
  match e with
  | Num n -> VInt n
  | Add1 e -> ( match eval env e with VInt n -> VInt (Int64.add n 1L))
  | Sub1 e -> ( match eval env e with VInt n -> VInt (Int64.sub n 1L))
  | Id x -> Env.find x env
  | Let (x, e1, e2) ->
      let v = eval env e1 in
      let env' = Env.add x v env in
      eval env' e2

