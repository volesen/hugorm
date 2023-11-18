open Asm
open Syntax
module Env = Map.Make (String)

type offset = int
and env = int Env.t

let add x env =
  let offset = Env.cardinal env + 1 in
  (Env.add x offset env, offset)

let rec compile_expr (env : env) (expr : expr) : asm =
  match expr with
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Add1 e -> compile_expr env e @ [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 e -> compile_expr env e @ [ IAdd (Reg RAX, Const (-1L)) ]
  | Id x ->
      let offset = Env.find x env in
      [ IMov (Reg RAX, RegOffset (RSP, -offset)) ]
  | Let (x, e1, e2) ->
      let env', offset = add x env in
      compile_expr env e1 (* Result in RAX *)
      @ [ IMov (RegOffset (RSP, -offset), Reg RAX) ] (* Move to stack *)
      @ compile_expr env' e2

let compile_program (program : program) : string =
  let env = Env.empty in
  let instrs = compile_expr env program in
  let asm_string = string_of_asm instrs in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
