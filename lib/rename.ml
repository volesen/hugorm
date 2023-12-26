open Syntax

type tag = int
type env = (string * string) list

let empty_env = []

exception UnboundId of string

let gen_id id tag = id ^ string_of_int tag

let rec rename_program (prog : tag program) : tag program =
  let decls = rename_decls empty_env prog.decls in
  let body = rename_expr empty_env prog.body in
  { decls; body }

and rename_decls env decls = List.map (rename_decl env) decls

and rename_decl env d =
  match d with
  | DFun (name, args, body, tag) ->
      (* Should we rename the function? *)
      DFun (name, args, rename_expr env body, tag)

and rename_expr (env : env) expr =
  match expr with
  | ENumber (n, tag) -> ENumber (n, tag)
  | EBool (b, tag) -> EBool (b, tag)
  | EPrim1 (op, e, tag) -> EPrim1 (op, rename_expr env e, tag)
  | EPrim2 (op, l, r, tag) ->
      EPrim2 (op, rename_expr env l, rename_expr env r, tag)
  | EIf (c, t, f, tag) ->
      EIf (rename_expr env c, rename_expr env t, rename_expr env f, tag)
  | EId (x, tag) -> (
      try EId (List.assoc x env, tag) with Not_found -> raise (UnboundId x))
  | ELet (x, e, b, tag) ->
      let x' = gen_id x tag in
      let env' = (x, x') :: env in
      ELet (x', rename_expr env e, rename_expr env' b, tag)
  | EApp (f, args, tag) -> EApp (f, List.map (rename_expr env) args, tag)
