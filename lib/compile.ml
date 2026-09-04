open Syntax
open Anf
open Asm
open Fvs

exception Unreachable of string

(* TODO: Make StringMap *)
(* TODO: Make StringSet*)
let find x env = try List.assoc x env with Not_found -> raise (Unreachable x)
let word_size = 8
let entry_label = "our_code_starts_here"
let const_true = Const 0xFFFFFFFFFFFFFFFFL
let const_false = Const 0x7FFFFFFFFFFFFFFFL
let bool_mask = Const 0x8000000000000000L
let bool_tag = Const 0x0000000000000003L
let tuple_tag = Const 0x0000000000000001L
let closure_tag = Const 0x0000000000000005L
let scratch_reg = Reg R11
let heap_reg = Reg R15

(** [count_let e] is the maximum number of nested let-bindings of any execution of [e] *)
let count_let (e : 'a aexpr) =
  let rec count_let_cexpr (cexpr : 'a cexpr) =
    match cexpr with
    | CIf (_, l, r, _) -> max (count_let_aexpr l) (count_let_aexpr r)
    (* Only contains immediate expression *)
    | CImmExpr _ | CPrim1 _ | CPrim2 _ | CApp _ | CTuple _ | CGetItem _
    | CLambda _ ->
        0
  and count_let_aexpr (aexpr : 'a aexpr) =
    match aexpr with
    | ALet (_, bind, body, _) ->
        max (count_let_cexpr bind) (1 + count_let_aexpr body)
    | ALetRec (bindings, body, _) ->
        let max_let_bindings =
          List.fold_left
            (fun acc (_, _, body, _) -> max acc (count_let_aexpr body))
            0 bindings
        in
        1 + max max_let_bindings (count_let_aexpr body)
    | ACExpr cexpr -> count_let_cexpr cexpr
  in
  count_let_aexpr e

(* We align heap and stack before function calls *)
let stack_alignment_padding n = n mod 16

(* Associative list mapping identifier to stack frame slot *)
(* TODO: Make this a module *)
type env = (string * int) list

let empty_env : env = []
let fresh_label ?(prefix = "L") tag = prefix ^ string_of_int tag

let rec compile_cexpr (env : env) (stack_index : int) (cexpr : tag cexpr) : asm
    =
  match cexpr with
  | CImmExpr immexpr -> compile_cimmexpr env immexpr
  | CPrim1 (op, e, _) -> compile_cprim1 env op e
  | CPrim2 (op, l, r, tag) -> compile_cprim2 env op l r tag
  | CIf (cond, thn, els, tag) -> compile_cif env stack_index cond thn els tag
  | CApp (f, args, _) -> compile_capp env f args
  | CTuple (elements, _) -> compile_ctuple env elements
  | CGetItem (tuple, index, _) -> compile_cgetitem env tuple index
  | CLambda (params, body, tag) ->
      compile_clambda env stack_index params body tag

and compile_immexpr (env : env) (immexpr : 'a immexpr) : arg =
  match immexpr with
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> RegOffset (RBP, find x env)

and compile_cimmexpr env immexpr =
  let arg = compile_immexpr env immexpr in
  [ IMov (Reg RAX, arg) ]

and compile_cprim1 env op e = compile_cimmexpr env e @ compile_cprim1_op op

and compile_cprim1_op op =
  match op with
  | Neg -> [ INeg (Reg RAX) ]
  | Not -> [ IMov (scratch_reg, bool_mask); IXor (Reg RAX, scratch_reg) ]

and compile_cprim2 env op l r tag =
  let l_arg = compile_immexpr env l in
  let r_arg = compile_immexpr env r in
  [ IMov (Reg RAX, l_arg); IMov (scratch_reg, r_arg) ]
  (* TODO: Make case swicth here *)
  @ compile_cprim2_op op tag

and compile_cprim2_op op tag =
  match op with
  | Add -> [ IAdd (Reg RAX, scratch_reg) ]
  | Sub -> [ ISub (Reg RAX, scratch_reg) ]
  | Mul -> [ IMul (Reg RAX, scratch_reg); ISar (Reg RAX, Const 1L) ]
  | And -> [ IAnd (Reg RAX, scratch_reg) ]
  | Or -> [ IOr (Reg RAX, scratch_reg) ]
  | Eq | Ne | Lt | Gt | Leq | Geq -> compile_cprim2_cmp_op op tag

and compile_cprim2_cmp_op cmp tag =
  let cmp_fail = fresh_label ~prefix:"cmp_fail" tag in
  [ ICmp (Reg RAX, scratch_reg); IMov (Reg RAX, const_true) ]
  @ (match cmp with
    | Eq -> [ IJe cmp_fail ]
    | Ne -> [ IJne cmp_fail ]
    | Lt -> [ IJl cmp_fail ]
    | Gt -> [ IJg cmp_fail ]
    | Leq -> [ IJle cmp_fail ]
    | Geq -> [ IJge cmp_fail ]
    | _ -> raise (Unreachable __LOC__))
  @ [ IMov (Reg RAX, const_false); ILabel cmp_fail ]

and compile_cif env stack_index cond thn els tag =
  let else_label = fresh_label ~prefix:"else" tag in
  let end_label = fresh_label ~prefix:"end" tag in
  compile_cimmexpr env cond
  @ [
      IMov (scratch_reg, const_false);
      ICmp (Reg RAX, scratch_reg);
      IJe else_label;
    ]
  @ compile_aexpr env stack_index thn
  @ [ IJmp end_label; ILabel else_label ]
  @ compile_aexpr env stack_index els
  @ [ ILabel end_label ]

and compile_capp env f args =
  let allocated = 8 * (1 + List.length args) in
  let padding = stack_alignment_padding allocated in
  let push_args_asm =
    args |> List.rev
    |> List.map (fun arg -> compile_cimmexpr env arg @ [ IPush (Reg RAX) ])
    |> List.concat
  in
  let pop_args_asm =
    [ IAdd (Reg RSP, Const (Int64.of_int (allocated + padding))) ]
  in
  let align_stack_asm =
    if padding <> 0 then [ ISub (Reg RSP, Const (Int64.of_int padding)) ]
    else []
  in
  align_stack_asm @ push_args_asm @ compile_cimmexpr env f
  @ [
      (* TODO: We assume f compiles to a closure *)
      IPush (Reg RAX);
      (* Remove the closure tag *)
      ISub (Reg RAX, closure_tag);
      (* Call the function pointer *)
      ICall (RegOffset (RAX, 0));
    ]
  @ pop_args_asm

and compile_ctuple env elements =
  let size = List.length elements in
  (* TODO: We need to maintain the invariant, that *)
  let padded_size = if size mod 2 = 0 then size + 1 else size in
  let move_elements_asm =
    elements
    |> List.mapi (fun i element ->
           let arg = compile_immexpr env element in
           [
             IMov (scratch_reg, arg);
             IMov (RegOffset (R15, 8 * (i + 1)), scratch_reg);
           ])
    |> List.concat
  in
  [
    (* Move size and value on the heap *)
    IMov (scratch_reg, Const (Int64.of_int size));
    IMov (RegOffset (R15, 0), scratch_reg);
  ]
  @ move_elements_asm
  @ [
      (* Tag the tuple *)
      IMov (Reg RAX, heap_reg);
      IAdd (Reg RAX, tuple_tag);
      (* Bump the header pointer *)
      IAdd (heap_reg, Const (Int64.of_int (8 * padded_size)));
    ]

and compile_cgetitem env tuple index =
  let tuple_arg = compile_immexpr env tuple in
  let index_arg = compile_immexpr env index in
  [
    IMov (Reg RAX, tuple_arg);
    (* Untag the tuple pointer *)
    ISub (Reg RAX, tuple_tag);
    (* Make a pointer *)
    IMov (scratch_reg, index_arg);
    ISar (scratch_reg, Const 1L);
    IMov (Reg RAX, RegBaseOffset (RAX, R11, 8, 8));
  ]

and allocate_closure num_fvs =
  let to_allocate =
    let size = 8 * (1 + num_fvs) in
    size + stack_alignment_padding size
  in
  [
    IMov (Reg RAX, heap_reg);
    IAdd (Reg RAX, closure_tag);
    IAdd (heap_reg, Const (Int64.of_int to_allocate));
  ]

and compile_closure env fvs tag =
  (* Assumes that the closure is already allocated, with the header pointer in RAX *)
  let label = fresh_label ~prefix:"lambda" tag in
  let move_fvs_to_heap_asm =
    fvs
    |> List.mapi (fun i fv ->
           [
             IMov (scratch_reg, RegOffset (RBP, find fv env));
             IMov (RegOffset (RAX, 8 * (i + 1)), scratch_reg);
           ])
    |> List.concat
  in
  [
    ISub (Reg RAX, closure_tag);
    ILea (scratch_reg, Label label);
    IMov (RegOffset (RAX, 0), scratch_reg);
  ]
  @ move_fvs_to_heap_asm
  @ [ IAdd (Reg RAX, closure_tag) ]

and compile_lambda_body env stack_index fvs params body tag =
  let lambda_label = fresh_label ~prefix:"lambda" tag in
  let lambda_end_label = fresh_label ~prefix:"lambda_end" tag in
  let frame_size =
    let max_stack_size = 8 * (count_let body + List.length fvs) in
    max_stack_size + stack_alignment_padding max_stack_size
  in
  (* Populate env with function arguments *)
  let env = List.mapi (fun i param -> (param, 8 * (i + 3))) params @ env in
  (* Populate env with free variables *)
  let env = List.mapi (fun i fv -> (fv, -8 * (i + 1))) fvs @ env in
  (* Move free variables to the stack *)
  let move_fvs_to_stack_asm =
    fvs
    |> List.mapi (fun i fv ->
           [
             IMov (Reg RAX, RegOffset (R11, 8 * (i + 1)));
             IMov (RegOffset (RBP, find fv env), Reg RAX);
           ])
    |> List.concat
  in
  [
    IJmp lambda_end_label;
    ILabel lambda_label;
    IPush (Reg RBP);
    IMov (Reg RBP, Reg RSP);
    ISub (Reg RSP, Const (Int64.of_int frame_size));
    (* Load the `self` pointer *)
    IMov (scratch_reg, RegOffset (RBP, 16));
    ISub (scratch_reg, closure_tag);
  ]
  @ move_fvs_to_stack_asm
  @ compile_aexpr env (stack_index - (8 * List.length fvs)) body
  @ [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ]
  @ [ ILabel lambda_end_label ]

and compile_clambda env stack_index params body tag =
  let fvs = S.elements (S.diff (fvs_aexpr body) (S.of_list params)) in
  compile_lambda_body env stack_index fvs params body tag
  @ allocate_closure (List.length fvs)
  @ compile_closure env fvs tag

and compile_body (body : tag aexpr) : asm =
  let frame_size =
    let max_stack_size = 8 * count_let body in
    max_stack_size + stack_alignment_padding max_stack_size
  in
  [
    ILabel entry_label;
    IPush (Reg RBP);
    IMov (Reg RBP, Reg RSP);
    ISub (Reg RSP, Const (Int64.of_int frame_size));
    (* Difference from function declarations *)
    IMov (heap_reg, Reg RDI);
  ]
  @ compile_aexpr empty_env 0 body
  @ [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ]

and compile_aexpr env stack_index aexpr =
  match aexpr with
  | ACExpr cexpr -> compile_cexpr env stack_index cexpr
  | ALet (x, bind, body, _) ->
      let stack_index' = stack_index - 8 in
      let env' = (x, stack_index') :: env in
      compile_cexpr env stack_index bind
      @ [ IMov (RegOffset (RBP, stack_index'), Reg RAX) ]
      @ compile_aexpr env' stack_index' body
  | ALetRec (bindings, body, _) ->
      let env' =
        List.mapi (fun i (name, _, _, _) -> (name, -8 * (i + 1))) bindings @ env
      in
      let stack_index' = stack_index - (8 * List.length bindings) in
      let allocate_closures_asm =
        bindings
        |> List.map (fun (x, params, body, tag) ->
               let fvs = S.diff (fvs_aexpr body) (S.of_list params) in
               allocate_closure (S.cardinal fvs)
               @ [ IMov (RegOffset (RBP, find x env'), Reg RAX) ]
               @ compile_lambda_body env' stack_index' (S.elements fvs) params
                   body tag)
        |> List.concat
      in
      let build_closure_asm =
        bindings
        |> List.map (fun (x, params, body, tag) ->
               let fvs = S.diff (fvs_aexpr body) (S.of_list params) in
               [ IMov (Reg RAX, RegOffset (RBP, find x env')) ]
               @ compile_closure env' (S.elements fvs) tag)
        |> List.concat
      in
      allocate_closures_asm @ build_closure_asm
      @ compile_aexpr env' stack_index' body

let compile_adecl (adecl : 'a adecl) : asm =
  match adecl with
  | ADFun (_, params, body, tag) -> compile_clambda empty_env 0 params body tag

let compile_aprog (aprog : 'a aprogram) : asm =
  let decls_asm = List.concat_map compile_adecl aprog.decls in
  let body_asm = compile_body aprog.body in
  [ ISection "text"; IExtern "error"; IExtern "print"; IGlobal entry_label ]
  @ body_asm @ decls_asm

let compile (prog : 'a program) : asm =
  Well_formedness.well_formed prog.body;
  let tagged = Syntax.tag prog in
  let renamed = Rename.rename_program tagged in
  (* print_endline (Syntax.show_program (fun _ _ -> ()) renamed) ; *)
  let anfed = Anf.anf_program renamed in
  let retagged = Anf.tag anfed in
  compile_aprog retagged
