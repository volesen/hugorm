open Asm
open Syntax
module Env = Map.Make (String)

type offset = int
and env = int Env.t

let add x env =
  let offset = Env.cardinal env + 1 in
  (Env.add x offset env, offset)

let err_unreachable = "Error: Unreachable"

type tag = int

let tag (e : 'a expr) : tag expr =
  let rec tag' (e : 'a expr) (cur : tag) : tag expr * tag =
    match e with
    | ENumber (n, _) -> (ENumber (n, cur), cur + 1)
    | EPrim1 (op, e, _) ->
        let e, cur = tag' e cur in
        (EPrim1 (op, e, cur), cur + 1)
    | EPrim2 (op, l, r, _) ->
        let l, cur = tag' l cur in
        let r, cur = tag' r cur in
        (EPrim2 (op, l, r, cur), cur + 1)
    | EId (x, _) -> (EId (x, cur), cur + 1)
    | ELet (x, e, body, _) ->
        let e, cur = tag' e cur in
        let body, cur = tag' body cur in
        (ELet (x, e, body, cur), cur + 1)
    | EIf (cond, thn, els, _) ->
        let cond, cur = tag' cond cur in
        let thn, cur = tag' thn cur in
        let els, cur = tag' els cur in
        (EIf (cond, thn, els, cur), cur + 1)
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
  | EPrim2 (_, l, r, _) -> is_imm l && is_imm r
  | ELet (_, e, body, _) -> is_anf e && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
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
    | EPrim2 (op, l, r, tag) ->
        let l_imm, l_ctx = to_anf' l in
        let r_imm, r_ctx = to_anf' r in
        let tmp = "$" ^ string_of_int tag in
        (EId (tmp, ()), l_ctx @ r_ctx @ [ (tmp, EPrim2 (op, l_imm, r_imm, ())) ])
    | ELet (x, e, body, _) ->
        let e_imm, e_ctx = to_anf' e in
        let body_imm, body_ctx = to_anf' body in
        (body_imm, e_ctx @ [ (x, e_imm) ] @ body_ctx)
    | EIf (cond, thn, els, tag) ->
        let cond_imm, cond_ctx = to_anf' cond in
        let thn_imm, thn_ctx = to_anf' thn in
        let els_imm, els_ctx = to_anf' els in
        let tmp = "$" ^ string_of_int tag in
        ( EId (tmp, ()),
          cond_ctx
          @ [
              ( tmp,
                EIf
                  (cond_imm, mk_let thn_imm thn_ctx, mk_let els_imm els_ctx, ())
              );
            ] )
  in
  let imm, ctx = to_anf' expr in
  mk_let imm ctx

let rename (e : tag expr) : tag expr =
  let rec rename' env e =
    match e with
    | ENumber (n, tag) -> ENumber (n, tag)
    | EId (x, tag) -> EId (Env.find x env, tag)
    | EPrim1 (op, e, tag) -> EPrim1 (op, rename' env e, tag)
    | EPrim2 (op, l, r, tag) -> EPrim2 (op, rename' env l, rename' env r, tag)
    | EIf (cond, thn, els, tag) ->
        EIf (rename' env cond, rename' env thn, rename' env els, tag)
    | ELet (x, e, body, tag) ->
        (* Rename the binding *)
        let x' = x ^ string_of_int tag in
        let env' = Env.add x x' env in
        (* Disallow recursion in the bind for now *)
        ELet (x', rename' env e, rename' env' body, tag)
  in
  rename' Env.empty e

let rec compile_expr (env : env) (expr : tag expr) : asm =
  match expr with
  | ENumber (n, _) -> [ IMov (Reg RAX, Const n) ]
  | EPrim1 (op, e, _) -> compile_prim1 env op e
  | EPrim2 (op, l, r, _) -> compile_prim2 env op l r
  | EId (x, _) -> compile_id env x
  | ELet (x, e, body, _) -> compile_let env x e body
  | EIf (cond, thn, els, tag) -> compile_if env cond thn els tag

and compile_prim1 env op e =
  (* TODO: Consider using compile_imm *)
  compile_expr env e
  @
  match op with
  | Add1 -> [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 -> [ IAdd (Reg RAX, Const (-1L)) ]

and compile_prim2 env op l r =
  let l_arg = compile_imm env l in
  let r_arg = compile_imm env r in
  [ IMov (Reg RAX, r_arg) ]
  @
  match op with
  | Add -> [ IAdd (Reg RAX, l_arg) ]
  | Sub -> [ ISub (Reg RAX, l_arg) ]
  | Mul -> [ IMul (Reg RAX, l_arg) ]

and compile_id env x =
  let offset = Env.find x env in
  [ IMov (Reg RAX, RegOffset (RSP, -offset)) ]

and compile_let env x e body =
  let env', offset = add x env in
  compile_expr env e
  @ [ IMov (RegOffset (RSP, -offset), Reg RAX) ]
  @ compile_expr env' body

and compile_if env cond thn els tag =
  let else_label = "if_else_" ^ string_of_int tag in
  let done_label = "if_done_" ^ string_of_int tag in
  (* TODO: Consider using compile_imm *)
  compile_expr env cond (* Result in RAX *)
  @ [ ICmp (Reg RAX, Const 0L); IJe else_label ]
  @ compile_expr env thn (* Compile then branch *)
  @ [ IJmp done_label; ILabel else_label ]
  @ compile_expr env els (* Compile else branch *)
  @ [ ILabel done_label ]

and compile_imm env e =
  match e with
  | ENumber (n, _) -> Const n
  | EId (x, _) ->
      let offset = Env.find x env in
      RegOffset (RSP, -offset)
  | _ -> failwith err_unreachable

let compile expr =
  let tagged = tag expr in
  let renamed = rename tagged in
  let anfed = tag (to_anf renamed) in
  let _ = assert (is_anf anfed) in
  (* Re-tag after ANF conversion *)
  let tagged = tag anfed in
  compile_expr Env.empty tagged

let compile_to_asm_string (program : 'a program) : string =
  let instrs = compile program in
  let asm_string = string_of_asm instrs in
  let prelude =
    "section .text\n" ^ "global our_code_starts_here\n"
    ^ "our_code_starts_here:"
  in
  let suffix = "  ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
