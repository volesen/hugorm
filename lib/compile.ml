open Asm
open Syntax
module Env = Map.Make (String)

type offset = int
and env = int Env.t

let add x env =
  let offset = Env.cardinal env + 1 in
  (Env.add x offset env, offset)

type tag = int

let tag (e : 'a expr) : tag expr =
  let rec tag' (e : 'a expr) (cur : tag) : tag expr * tag =
    match e with
    | ENumber (n, _) -> (ENumber (n, cur), cur + 1)
    | EPrim1 (op, e, _) ->
        let e', cur = tag' e cur in
        (EPrim1 (op, e', cur), cur + 1)
    | EPrim2 (op, e1, e2, _) ->
        let e1', cur = tag' e1 cur in
        let e2', cur = tag' e2 cur in
        (EPrim2 (op, e1', e2', cur), cur + 1)
    | EId (x, _) -> (EId (x, cur), cur + 1)
    | ELet (x, e1, e2, _) ->
        let e1', cur = tag' e1 cur in
        let e2', cur = tag' e2 cur in
        (ELet (x, e1', e2', cur), cur + 1)
    | EIf (e1, e2, e3, _) ->
        let e1', cur = tag' e1 cur in
        let e2', cur = tag' e2 cur in
        let e3', cur = tag' e3 cur in
        (EIf (e1', e2', e3', cur), cur + 1)
  in
  let tagged, _ = tag' e 1 in
  tagged

let rec compile_expr (env : env) (expr : tag expr) : asm =
  match expr with
  | ENumber (n, _) -> [ IMov (Reg RAX, Const n) ]
  | EPrim1 (op, e, _) -> compile_prim1 env op e
  | EId (x, _) -> compile_id env x
  | ELet (x, e1, e2, _) -> compile_let env x e1 e2
  | EIf (e1, e2, e3, tag) -> compile_if env e1 e2 e3 tag
  | _ -> failwith "TODO"

and compile_prim1 env op e =
  match op with
  | Add1 -> compile_expr env e @ [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 -> compile_expr env e @ [ IAdd (Reg RAX, Const (-1L)) ]

and compile_id env x =
  let offset = Env.find x env in
  [ IMov (Reg RAX, RegOffset (RSP, -offset)) ]

and compile_let env x e1 e2 =
  let env', offset = add x env in
  compile_expr env e1 (* Result in RAX *)
  @ [ IMov (RegOffset (RSP, -offset), Reg RAX) ] (* Move to stack *)
  @ compile_expr env' e2

and compile_if env cond thn els tag =
  let else_label = "if_else_" ^ string_of_int tag in
  let done_label = "if_done_" ^ string_of_int tag in
  compile_expr env cond (* Result in RAX *)
  @ [ ICmp (Reg RAX, Const 0L); IJe else_label ]
  @ compile_expr env thn (* Compile then branch *)
  @ [ IJmp done_label; ILabel else_label ]
  @ compile_expr env els (* Compile else branch *)
  @ [ ILabel done_label ]

let compile e =
  let tagged = tag e in
  let env = Env.empty in
  let compiled = compile_expr env tagged in
  compiled

let compile_program (program : 'a program) : string =
  let instrs = compile program in
  let asm_string = string_of_asm instrs in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
