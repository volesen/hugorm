open Syntax

type tag = int
type env = (string * string) list

let empty_env = []

exception UnboundId of string

let gen_id id tag = id ^ string_of_int tag

let rec rename_program (prog : tag program) : tag program =
  let decls = rename_decls prog.decls in
  let body = rename_expr empty_env prog.body in
  { decls; body }

and rename_decls decls = List.map rename_decl decls

and rename_decl d =
  match d with
  | DFun (name, args, body, tag) ->
      (* Should we rename the arguments? *)
      let env = List.map (fun arg -> (arg, arg)) args in
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
  | EApp (f, args, tag) ->
      EApp (rename_expr env f, List.map (rename_expr env) args, tag)
  | ETuple (elements, tag) -> ETuple (List.map (rename_expr env) elements, tag)
  | EGetItem (tuple, index, tag) ->
      EGetItem (rename_expr env tuple, rename_expr env index, tag)
  | ELambda (params, body, tag) ->
      (* TODO: rename arguments *)
      let env, params =
        List.fold_left
          (fun (env, params) param ->
            let param' = gen_id param tag in
            ((param, param') :: env, param' :: params))
          (env, []) params
      in
      let params = List.rev params in
      ELambda (params, rename_expr env body, tag)
  | ELetRec (x, e, b, tag) ->
      let x' = gen_id x tag in
      let env' = (x, x') :: env in
      ELetRec (x', rename_expr env' e, rename_expr env' b, tag)
