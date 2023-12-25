open Asm
open Syntax

exception Unreachable of string
exception Integer_overflow of string
exception Argument_mismatch of string

let entry_label = "our_code_starts_here"
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let const_true = Const 0xFFFFFFFFFFFFFFFFL
let const_false = Const 0x7FFFFFFFFFFFFFFFL
let bool_mask = Const 0x8000000000000000L
let bool_tag = Const 0x0000000000000001L
let scratch_reg = Reg R11
let err_code_not_a_number = Const 1L
let err_code_not_a_boolean = Const 2L

(* Associative list mapping identifiers to stack offsets *)
type env = (string * int) list

let empty_env : env = []

(* *)
let stack_alignment_padding (stack_index : int) : int = stack_index mod 16

(* [count_let e] returns the deepest nesting of let-bindings in [e]. *)
let rec count_let (e : 'a expr) : int =
  match e with
  | ENumber _ | EBool _ | EId _ -> 0
  | EPrim1 (_, e, _) -> count_let e
  | EPrim2 (_, l, r, _) -> max (count_let l) (count_let r)
  | ELet (_, e, body, _) -> max (count_let e) (1 + count_let body)
  | EIf (cond, thn, els, _) ->
      max (count_let cond) (max (count_let thn) (count_let els))
  | EApp (_, args, _) -> List.fold_left max 0 (List.map count_let args)

let gen_label ?(prefix = "L") tag = prefix ^ string_of_int tag

let rec compile_program (prog : tag program) : asm =
  let decls_asm = List.concat_map compile_decl prog.decls in
  let body_asm = compile_body prog.body in
  [ ISection "text"; IExtern "error"; IExtern "print"; IGlobal entry_label ]
  @ body_asm @ decls_asm

and compile_decl (decl : tag decl) : asm =
  match decl with
  | DFun (name, args, body, _) ->
      let max_stack_size = reg64_size_bytes * count_let body in
      let stack_frame_size =
        max_stack_size + stack_alignment_padding max_stack_size
      in
      let env = List.mapi (fun i a -> (a, (i + 2) * reg64_size_bytes)) args in
      let body_asm = compile_expr env 0 body in
      [
        ILabel name;
        IPush (Reg RBP);
        IMov (Reg RBP, Reg RSP);
        ISub (Reg RSP, Const (Int64.of_int stack_frame_size));
      ]
      @ body_asm
      @ [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ]

and compile_body (body : tag expr) : asm =
  (* We exploit that the entry point is always a function with no arguments. *)
  compile_decl (DFun (entry_label, [], body, 0))

and compile_imm env e =
  match e with
  | ENumber (n, _) ->
      if n > max_int || n < min_int then
        raise (Integer_overflow "Number too large")
      else Const (Int64.shift_left n 1)
  | EBool (true, _) -> const_true
  | EBool (false, _) -> const_false
  | EId (x, _) -> (
      try
        let offset = List.assoc x env in
        RegOffset (RBP, offset)
      with Not_found -> raise (Unreachable __LOC__))
  | _ -> raise (Unreachable __LOC__)

and compile_expr (env : env) (stack_index : int) (e : tag expr) : asm =
  match e with
  | (ENumber _ | EBool _ | EId _) as e -> [ IMov (Reg RAX, compile_imm env e) ]
  | EPrim1 (op, e, _) -> compile_prim1 env stack_index op e
  | EPrim2 (op, l, r, tag) -> compile_prim2 env stack_index op l r tag
  | ELet (x, e, body, _) -> compile_let env stack_index x e body
  | EIf (cond, thn, els, tag) -> compile_if env stack_index cond thn els tag
  (* We use a stack-based calling convention. C FFI is handled manually for now. *)
  | EApp ((("print" | "error") as f), args, _) ->
      compile_native_call env stack_index f args
  | EApp (f, args, _) -> compile_app env stack_index f args

and compile_prim1 env stack_index op e =
  compile_expr env stack_index e
  @
  match op with
  | Neg -> [ INeg (Reg RAX) ]
  | Not -> [ IMov (scratch_reg, bool_mask); IXor (Reg RAX, scratch_reg) ]

and compile_prim2 env _ op l r tag =
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
      let cmp_fail = gen_label ~prefix:"cmp_fail" tag in
      let jump_instr =
        match cmp with
        | Less -> IJl cmp_fail
        | Greater -> IJg cmp_fail
        | LessEq -> IJle cmp_fail
        | GreaterEq -> IJge cmp_fail
        | Eq -> IJe cmp_fail
        | Ne -> IJne cmp_fail
        | _ -> raise (Unreachable __LOC__)
      in
      [
        ICmp (Reg RAX, scratch_reg);
        IMov (Reg RAX, const_true);
        jump_instr;
        IMov (Reg RAX, const_false);
        ILabel cmp_fail;
      ]

and compile_let env stack_index x e body =
  let stack_index' = stack_index - reg64_size_bytes in
  let env' = (x, stack_index') :: env in
  compile_expr env stack_index e
  @ [ IMov (RegOffset (RBP, stack_index'), Reg RAX) ]
  @ compile_expr env' stack_index' body

and compile_if env stack_index cond thn els tag =
  let else_label = gen_label ~prefix:"else" tag in
  let end_label = gen_label ~prefix:"end" tag in
  compile_expr env stack_index cond
  @ [
      IMov (scratch_reg, const_false);
      ICmp (Reg RAX, scratch_reg);
      IJe else_label;
    ]
  @ compile_expr env stack_index thn
  @ [ IJmp end_label; ILabel else_label ]
  @ compile_expr env stack_index els
  @ [ ILabel end_label ]

and compile_app env _ f args =
  let allocated = reg64_size_bytes * List.length args in
  let padding = stack_alignment_padding allocated in
  let args_asm =
    args
    |> List.rev_map (fun arg ->
           let arg_asm = compile_imm env arg in
           [ IMov (Reg RAX, arg_asm); IPush (Reg RAX) ])
    |> List.concat
  in
  [ ISub (Reg RSP, Const (Int64.of_int padding)) ]
  @ args_asm
  @ [ ICall f; IAdd (Reg RSP, Const (Int64.of_int (allocated + padding))) ]

and compile_native_call env _ f args =
  match (f, args) with
  | "print", [ e ] ->
      let e_imm = compile_imm env e in
      [ IMov (Reg RDI, e_imm); ICall "print" ]
  | "error", [ err_code; err_val ] ->
      let err_val_imm = compile_imm env err_val in
      let err_code_imm = compile_imm env err_code in
      [
        IMov (Reg RDI, err_code_imm); IMov (Reg RSI, err_val_imm); ICall "error";
      ]
  | _ -> raise (Argument_mismatch ("Invalid native call: " ^ f))

let compile (prog : 'a program) : asm =
  let tagged_prog = Tag.tag prog in
  let anfed_prog = Anf.anf tagged_prog in
  let retagged_prog = Tag.tag anfed_prog in
  compile_program retagged_prog
