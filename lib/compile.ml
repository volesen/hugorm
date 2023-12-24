open Asm
open Syntax

let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let const_true = Const 0xFFFFFFFFFFFFFFFFL
let const_false = Const 0x7FFFFFFFFFFFFFFFL
let bool_mask = Const 0x8000000000000000L
let bool_tag = Const 0x0000000000000001L
let scratch_reg = Reg R11
let err_code_not_a_number = Const 1L
let err_code_not_a_boolean = Const 2L
let err_unreachable = "Error: Unreachable"
let err_integer_overflow = "Error: Integer overflow"

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
    | EBool (b, _) -> (EBool (b, cur), cur + 1)
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

let rename (e : tag expr) : tag expr =
  let rec rename' env e =
    match e with
    | ENumber _ as e -> e
    | EBool _ as e -> e
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

(* Is expression immediate? *)
let is_imm expr =
  match expr with
  | ENumber _ | EBool _ | EId _ -> true
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
    | EBool (b, _) -> (EBool (b, ()), [])
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

let assert_number reg =
  [
    IMov (scratch_reg, bool_tag);
    ITest (Reg reg, scratch_reg);
    (* We only have two types. If it is not a boolean, it must tbe number *)
    IJnz "err_not_a_number";
  ]

let assert_boolean reg =
  [
    IMov (scratch_reg, bool_tag);
    ITest (Reg reg, scratch_reg);
    IJz "err_not_a_boolean";
  ]

let rec compile_expr (env : env) (expr : tag expr) : asm =
  match expr with
  | (ENumber _ | EBool _ | EId _) as e -> [ IMov (Reg RAX, compile_imm env e) ]
  | EPrim1 (op, e, _) -> compile_prim1 env op e
  | EPrim2 (op, l, r, tag) -> compile_prim2 env op l r tag
  | ELet (x, e, body, _) -> compile_let env x e body
  | EIf (cond, thn, els, tag) -> compile_if env cond thn els tag

and compile_imm env e =
  match e with
  | ENumber (n, _) ->
      if n > max_int || n < min_int then failwith err_integer_overflow
      else Const (Int64.shift_left n 1)
  | EBool (true, _) -> const_true
  | EBool (false, _) -> const_false
  | EId (x, _) ->
      let offset = Env.find x env in
      RegOffset (RBP, -offset)
  | _ -> failwith err_unreachable

and compile_prim1 env op e =
  let arg = compile_imm env e in
  [ IMov (Reg RAX, arg) ]
  @
  match op with
  | Neg -> assert_number RAX @ [ INeg (Reg RAX) ]
  | Not ->
      assert_boolean RAX
      @ [ IMov (scratch_reg, bool_mask); IXor (Reg RAX, scratch_reg) ]

and compile_prim2 env op l r tag =
  let l_arg = compile_imm env l in
  let r_arg = compile_imm env r in
  [ IMov (Reg RAX, l_arg); IMov (scratch_reg, r_arg) ]
  @
  match op with
  | Add -> [ IAdd (Reg RAX, scratch_reg) ]
  | Sub -> [ ISub (Reg RAX, scratch_reg) ]
  | Mul -> [ IMul (Reg RAX, scratch_reg); ISar (Reg RAX, Const 1L) ]
  | And -> [ IAnd (Reg RAX, scratch_reg) ]
  | Or -> [ IOr (Reg RAX, scratch_reg) ]
  | (Less | Greater | LessEq | GreaterEq | Eq | Ne) as cmp ->
      let cmp_fail = "cmp_fail_" ^ string_of_int tag in
      let jump_instr =
        match cmp with
        | Less -> IJl cmp_fail
        | Greater -> IJg cmp_fail
        | LessEq -> IJle cmp_fail
        | GreaterEq -> IJge cmp_fail
        | Eq -> IJe cmp_fail
        | Ne -> IJne cmp_fail
        | _ -> failwith err_unreachable
      in
      [
        ICmp (Reg RAX, scratch_reg);
        (* Assume the result is true *)
        IMov (Reg RAX, const_true);
        jump_instr;
        (* the comparison failed *)
        IMov (Reg RAX, const_false);
        ILabel cmp_fail;
      ]

and compile_id env x =
  let offset = Env.find x env in
  [ IMov (Reg RAX, RegOffset (RBP, -offset)) ]

and compile_let env x e body =
  let env', offset = add x env in
  compile_expr env e
  @ [ IMov (RegOffset (RBP, -offset), Reg RAX) ]
  @ compile_expr env' body

and compile_if env cond thn els tag =
  let else_label = "if_else_" ^ string_of_int tag in
  let done_label = "if_done_" ^ string_of_int tag in
  let arg = compile_imm env cond in
  [ IMov (Reg RAX, arg) ]
  @ assert_boolean RAX
  @ [
      IMov (Reg RAX, arg);
      IMov (scratch_reg, const_false);
      ICmp (Reg RAX, scratch_reg);
      IJe else_label;
    ]
  @ compile_expr env thn
  @ [ IJmp done_label; ILabel else_label ]
  @ compile_expr env els @ [ ILabel done_label ]

let rec count_let (e : 'a expr) : int =
  (* In ANF form *)
  match e with
  | ENumber _ | EBool _ | EId _ -> 0
  | EPrim1 (_, e, _) -> count_let e (* Should be 0 as we are in ANF *)
  | EPrim2 (_, l, r, _) ->
      count_let l + count_let r (* Should be 0 as we are in ANF *)
  | EIf (cond, thn, els, _) ->
      count_let cond + max (count_let thn) (count_let els)
  | ELet (_, e, body, _) -> 1 + max (count_let e) (count_let body)

let error_handler label err_code =
  [
    ILabel label;
    IMov (Reg RSI, Reg RAX);
    IMov (Reg RDI, Const err_code);
    ICall "error";
  ]

let align n alignment =
  let remainder = n mod alignment in
  if remainder = 0 then n else n + alignment - remainder

let stack_align n = align n 16

let compile_to_asm_string (expr : 'a expr) : string =
  let tagged = tag expr in
  let renamed = rename tagged in
  let anfed = tag (to_anf renamed) in
  let _ = assert (is_anf anfed) in
  (* Count let-bindings to allocate stack frame size *)
  let max_locals = count_let anfed in
  let stack_size = stack_align (max_locals * 8) in
  (* Re-tag after ANF conversion *)
  let tagged = tag anfed in
  let body = compile_expr Env.empty tagged in
  let prelude =
    [
      ISection "text";
      IExtern "error";
      IExtern "print";
      IGlobal "our_code_starts_here";
      ILabel "our_code_starts_here";
      IPush (Reg RBX);
      IMov (Reg RBP, Reg RSP);
      ISub (Reg RSP, Const (Int64.of_int stack_size));
    ]
  in
  let postlude = [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ] in
  let asm =
    prelude @ body @ postlude
    @ error_handler "err_not_a_number" 1L
    @ error_handler "err_not_a_boolean" 2L
  in
  string_of_asm asm
