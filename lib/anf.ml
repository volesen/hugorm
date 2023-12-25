open Syntax

(* [is_imm expr] returns true if [expr] is an immediate expression *)
let is_imm expr =
  match expr with
  | ENumber _ | EBool _ | EId _ -> true
  | EPrim1 _ | EPrim2 _ | ELet _ | EIf _ | EApp _ -> false

(* [is_anf expr] returns true if [expr] is in administrative normal form (ANF) *)
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

(* [gen_id tag] generates a fresh identifier with the given tag *)
let gen_id tag = "x" ^ string_of_int tag

let rec anf_program (prog : tag program) : unit program =
  let anfed_decls = List.map anf_decl prog.decls in
  let anfed_body, ctx = anf_expr prog.body in
  { decls = anfed_decls; body = mk_let anfed_body ctx }

and anf_decl (decl : tag decl) : unit decl =
  match decl with
  | DFun (name, args, body, _) ->
      let body, ctx = anf_expr body in
      DFun (name, args, mk_let body ctx, ())

and anf_expr (expr : tag expr) : unit expr * (string * unit expr) list =
  match expr with
  | ENumber (n, _) -> (ENumber (n, ()), [])
  | EBool (b, _) -> (EBool (b, ()), [])
  | EId (x, _) -> (EId (x, ()), [])
  | EPrim1 (op, e, tag) ->
      let imm, ctx = anf_expr e in
      let x = gen_id tag in
      (EId (x, ()), ctx @ [ (x, EPrim1 (op, imm, ())) ])
  | EPrim2 (op, l, r, tag) ->
      let l_imm, l_ctx = anf_expr l in
      let r_imm, r_ctx = anf_expr r in
      let x = gen_id tag in
      (EId (x, ()), l_ctx @ r_ctx @ [ (x, EPrim2 (op, l_imm, r_imm, ())) ])
  | ELet (x, e, body, _) ->
      let imm, ctx = anf_expr e in
      let body, body_ctx = anf_expr body in
      (body, ctx @ [ (x, imm) ] @ body_ctx)
  | EIf (cond, thn, els, tag) ->
      let cond_imm, cond_ctx = anf_expr cond in
      let thn_imm, thn_ctx = anf_expr thn in
      let els_imm, els_ctx = anf_expr els in
      let x = gen_id tag in
      ( EId (x, ()),
        cond_ctx
        @ [
            ( x,
              EIf (cond_imm, mk_let thn_imm thn_ctx, mk_let els_imm els_ctx, ())
            );
          ] )
  | EApp (f, args, tag) ->
      let imm_args, ctxs_args = List.split (List.map anf_expr args) in
      let ctx_args = List.concat ctxs_args in
      let x = gen_id tag in
      (EId (x, ()), ctx_args @ [ (x, EApp (f, imm_args, ())) ])

let anf (prog : tag program) : unit program = anf_program prog
