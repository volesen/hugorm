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

type env = int Env.t
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
    | EApp (f, args, _) ->
        let args, cur =
          List.fold_left
            (fun (args, cur) arg ->
              let arg, cur = tag' arg cur in
              (args @ [ arg ], cur))
            ([], cur) args
        in
        (EApp (f, args, cur), cur + 1)
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
    | EApp (f, args, tag) -> EApp (f, List.map (rename' env) args, tag)
  in
  rename' Env.empty e

(* Is expression immediate? *)
let is_imm expr =
  match expr with
  | ENumber _ | EBool _ | EId _ -> true
  | EPrim1 _ | EPrim2 _ | ELet _ | EIf _ | EApp _ -> false

(* Are all operands are immediate? *)
let rec is_anf expr =
  match expr with
  | ENumber _ | EBool _ | EId _ -> true
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, l, r, _) -> is_imm l && is_imm r
  | ELet (_, e, body, _) -> is_anf e && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | EApp (_, args, _) -> List.for_all is_imm args

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
    | EApp (f, args, tag) ->
        let args_imm, args_ctx =
          List.fold_left
            (fun (args_imm, args_ctx) arg ->
              let arg_imm, arg_ctx = to_anf' arg in
              (args_imm @ [ arg_imm ], args_ctx @ arg_ctx))
            ([], []) args
        in
        let tmp = "$" ^ string_of_int tag in
        (EId (tmp, ()), args_ctx @ [ (tmp, EApp (f, args_imm, ())) ])
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

let split_n n lst =
  let rec loop n acc lst =
    if n = 0 then (List.rev acc, lst)
    else
      match lst with
      | [] -> failwith err_unreachable
      | x :: xs -> loop (n - 1) (x :: acc) xs
  in
  loop n [] lst

let align n alignment =
  let remainder = n mod alignment in
  if remainder = 0 then n else n + alignment - remainder

let stack_align n = align n 16

let rec compile_expr (env : env) (stack_index : int) (expr : tag expr) : asm =
  match expr with
  | (ENumber _ | EBool _ | EId _) as e ->
      [ IMov (Reg RAX, compile_imm env stack_index e) ]
  | EPrim1 (op, e, _) -> compile_prim1 env stack_index op e
  | EPrim2 (op, l, r, tag) -> compile_prim2 env stack_index op l r tag
  | ELet (x, e, body, _) -> compile_let env stack_index x e body
  | EIf (cond, thn, els, tag) -> compile_if env stack_index cond thn els tag
  | EApp (f, args, _) -> compile_app env stack_index f args

and compile_imm env _ e =
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

and compile_prim1 env stack_index op e =
  let arg = compile_imm env stack_index e in
  [ IMov (Reg RAX, arg) ]
  @
  match op with
  | Neg -> assert_number RAX @ [ INeg (Reg RAX) ]
  | Not ->
      assert_boolean RAX
      @ [ IMov (scratch_reg, bool_mask); IXor (Reg RAX, scratch_reg) ]

and compile_prim2 env stack_index op l r tag =
  let l_arg = compile_imm env stack_index l in
  let r_arg = compile_imm env stack_index r in
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

and compile_id env _ x =
  let offset = Env.find x env in
  [ IMov (Reg RAX, RegOffset (RBP, -offset)) ]

and compile_let env stack_index x e body =
  let stack_index' = stack_index + reg64_size_bytes in
  let env' = Env.add x stack_index' env in
  compile_expr env stack_index e
  @ [ IMov (RegOffset (RBP, -stack_index'), Reg RAX) ]
  @ compile_expr env' stack_index' body

and compile_if env stack_index cond thn els tag =
  let else_label = "if_else_" ^ string_of_int tag in
  let done_label = "if_done_" ^ string_of_int tag in
  let arg = compile_imm env stack_index cond in
  [ IMov (Reg RAX, arg) ]
  @ assert_boolean RAX
  @ [
      IMov (Reg RAX, arg);
      IMov (scratch_reg, const_false);
      ICmp (Reg RAX, scratch_reg);
      IJe else_label;
    ]
  @ compile_expr env stack_index thn
  @ [ IJmp done_label; ILabel else_label ]
  @ compile_expr env stack_index els
  @ [ ILabel done_label ]

and compile_app env stack_index f args =
  let reg_args, stack_args = split_n 6 args in
  (* The first 6 arguments are passed in registers *)
  let mov_reg_args =
    List.mapi
      (fun i arg ->
        let reg = List.nth arg_passing_regs i in
        let arg = compile_imm env stack_index arg in
        IMov (Reg reg, arg))
      reg_args
  in
  (* The rest are passed on the stack (in reverse order) *)
  let push_stack_args =
    List.rev_map
      (fun arg ->
        let arg = compile_imm env stack_index arg in
        IPush arg)
      stack_args
  in
  (* We need to align the stack to 16 bytes before calling *)
  (* TODO: Quick and dirty. Probably use `sub RSP, padding` *)
  let push_stack_args =
    if List.length push_stack_args mod 2 = 0 then push_stack_args
    else push_stack_args @ [ IPush (Reg RAX) ]
  in
  let pop_stack_args =
    [
      IAdd
        ( Reg RSP,
          Const (Int64.of_int (reg64_size_bytes * List.length push_stack_args))
        );
    ]
  in
  mov_reg_args @ push_stack_args @ [ ICall f ] @ pop_stack_args

(* [count_let e] returns t  he deepest nesting of let-bindings in [e] *)
let rec count_let (e : 'a expr) : int =
  match e with
  | ENumber _ | EBool _ | EId _ -> 0
  | EPrim1 (_, e, _) -> count_let e
  | EPrim2 (_, l, r, _) -> max (count_let l) (count_let r)
  | ELet (_, e, body, _) -> max (count_let e) (1 + count_let body)
  | EIf (cond, thn, els, _) ->
      max (count_let cond) (max (count_let thn) (count_let els))
  | EApp (_, args, _) -> List.fold_left max 0 (List.map count_let args)

let error_handler label err_code =
  [
    ILabel label;
    IMov (Reg RSI, Reg RAX);
    IMov (Reg RDI, Const err_code);
    ICall "error";
  ]

let compile_to_asm_string (expr : 'a expr) : string =
  let tagged = tag expr in
  let renamed = rename tagged in
  let anfed = tag (to_anf renamed) in
  let _ = assert (is_anf anfed) in
  (* Count let-bindings to allocate stack frame size *)
  let max_locals = count_let anfed in
  let stack_frame_size = stack_align (max_locals * reg64_size_bytes) in
  (* Re-tag after ANF conversion *)
  let tagged = tag anfed in
  let body = compile_expr Env.empty 0 tagged in
  let prelude =
    [
      ISection "text";
      IExtern "error";
      IExtern "print";
      IGlobal "our_code_starts_here";
      ILabel "our_code_starts_here";
      IPush (Reg RBX);
      IMov (Reg RBP, Reg RSP);
      ISub (Reg RSP, Const (Int64.of_int stack_frame_size));
    ]
  in
  let postlude = [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ] in
  let asm =
    prelude @ body @ postlude
    @ error_handler "err_not_a_number" 1L
    @ error_handler "err_not_a_boolean" 2L
  in
  string_of_asm asm
