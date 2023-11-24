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

(* Is expression immediate? *)
let is_imm expr =
  match expr with
  | ENumber _ -> true
  | EId _ -> true
  | EPrim1 _ | EPrim2 _ | ELet _ | EIf _ -> false

(* Are all operands are immediate? *)
let rec is_anf expr =
  match expr with
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet (_, e1, e2, _) -> is_anf e1 && is_anf e2
  | EIf (e1, e2, e3, _) -> is_imm e1 && is_anf e2 && is_anf e3
  | e -> is_imm e

(** [mk_let expr bindings] encloses [expr] in nested let bindings *)
let mk_let expr bindings =
  List.fold_right (fun (x, e) body -> ELet (x, e, body, ())) bindings expr

let to_anf expr =
  let rec to_anf' (expr : tag expr) : unit expr * (string * unit expr) list =
    match expr with
    | ENumber (n, _) -> (ENumber (n, ()), [])
    | EId (x, _) -> (EId (x, ()), [])
    | EPrim1 (op, e, tag) ->
        let imm, ctx = to_anf' e in
        let tmp = "$" ^ string_of_int tag in
        (EId (tmp, ()), ctx @ [ (tmp, EPrim1 (op, imm, ())) ])
    | EPrim2 (op, e1, e2, tag) ->
        let e1_imm, e1_ctx = to_anf' e1 in
        let e2_imm, e2_ctx = to_anf' e2 in
        let tmp = "$" ^ string_of_int tag in
        ( EId (tmp, ()),
          e1_ctx @ e2_ctx @ [ (tmp, EPrim2 (op, e1_imm, e2_imm, ())) ] )
    | ELet (x, e1, e2, _) ->
        let e1_imm, e1_ctx = to_anf' e1 in
        let e2_imm, e2_ctx = to_anf' e2 in
        (e2_imm, e1_ctx @ [ (x, e1_imm) ] @ e2_ctx)
    | EIf (e1, e2, e3, tag) ->
        let e1_imm, e1_ctx = to_anf' e1 in
        let e2_imm, e2_ctx = to_anf' e2 in
        let e3_imm, e3_ctx = to_anf' e3 in
        let tmp = "$" ^ string_of_int tag in
        ( EId (tmp, ()),
          e1_ctx
          @ [
              (tmp, EIf (e1_imm, mk_let e2_imm e2_ctx, mk_let e3_imm e3_ctx, ()));
            ] )
  in
  let imm, ctx = to_anf' expr in
  mk_let imm ctx

let rec compile_expr (env : env) (expr : tag expr) : asm =
  match expr with
  | ENumber (n, _) -> [ IMov (Reg RAX, Const n) ]
  | EPrim1 (op, e, _) -> compile_prim1 env op e
  | EPrim2 (op, l, r, _) -> compile_prim2 env op l r
  | EId (x, _) -> compile_id env x
  | ELet (x, e1, e2, _) -> compile_let env x e1 e2
  | EIf (e1, e2, e3, tag) -> compile_if env e1 e2 e3 tag

and compile_prim1 env op e =
  match op with
  | Add1 -> compile_expr env e @ [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 -> compile_expr env e @ [ IAdd (Reg RAX, Const (-1L)) ]

and compile_prim2 _ _ _ _ =
  (* As the program is in ANF, I know that `e1` and `e2` are immediate values *)
  failwith "TODO"

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
    "section .text\n" ^ "global our_code_starts_here\n"
    ^ "our_code_starts_here:"
  in
  let suffix = "  ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
