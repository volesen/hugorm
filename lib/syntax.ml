type 'a program = { decls : 'a decl list; body : 'a expr } [@@deriving show]
and 'a decl = DFun of string * string list * 'a expr * 'a

and 'a expr =
  | ENumber of int64 * 'a
  | EBool of bool * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EId of string * 'a
  | ELet of string * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | EApp of 'a expr * 'a expr list * 'a
  | ETuple of 'a expr list * 'a
  | EGetItem of 'a expr * 'a expr * 'a
  | ELambda of string list * 'a expr * 'a
  | ELetRec of (string * string list * 'a expr * 'a) list * 'a expr * 'a

and prim1 = Neg | Not
and prim2 = Add | Sub | Mul | And | Or | Lt | Gt | Leq | Geq | Eq | Ne

type tag = int

let tag_of_expr (e : 'a expr) : 'a =
  match e with
  | ENumber (_, tag) -> tag
  | EBool (_, tag) -> tag
  | EPrim1 (_, _, tag) -> tag
  | EPrim2 (_, _, _, tag) -> tag
  | EId (_, tag) -> tag
  | ELet (_, _, _, tag) -> tag
  | EIf (_, _, _, tag) -> tag
  | EApp (_, _, tag) -> tag
  | ETuple (_, tag) -> tag
  | EGetItem (_, _, tag) -> tag
  | ELambda (_, _, tag) -> tag
  | ELetRec (_, _, tag) -> tag

let rec tag_program (prog : 'a program) : tag program =
  let decls, tag = tag_decls prog.decls 0 in
  let body, _ = tag_expr prog.body tag in
  { decls; body }

and tag_decls decls tag =
  let decls, tag =
    List.fold_left
      (fun (decls, tag) decl ->
        let decl, tag = tag_decl decl tag in
        (decl :: decls, tag))
      ([], tag) decls
  in
  (List.rev decls, tag)

and tag_decl d tag =
  match d with
  | DFun (name, args, body, _) ->
      let body, tag = tag_expr body tag in
      (DFun (name, args, body, tag), tag + 1)

and tag_expr e tag =
  match e with
  | ENumber (n, _) -> (ENumber (n, tag), tag + 1)
  | EBool (b, _) -> (EBool (b, tag), tag + 1)
  | EPrim1 (op, e, _) ->
      let e, tag = tag_expr e tag in
      (EPrim1 (op, e, tag), tag + 1)
  | EPrim2 (op, e1, e2, _) ->
      let e1, tag = tag_expr e1 tag in
      let e2, tag = tag_expr e2 tag in
      (EPrim2 (op, e1, e2, tag), tag + 1)
  | EId (x, _) -> (EId (x, tag), tag + 1)
  | ELet (x, e1, e2, _) ->
      let e1, tag = tag_expr e1 tag in
      let e2, tag = tag_expr e2 tag in
      (ELet (x, e1, e2, tag), tag + 1)
  | EIf (cond, thn, els, _) ->
      let cond, tag = tag_expr cond tag in
      let thn, tag = tag_expr thn tag in
      let els, tag = tag_expr els tag in
      (EIf (cond, thn, els, tag), tag + 1)
  | EApp (f, args, _) ->
      let f, tag = tag_expr f tag in
      let args, tag = tag_args args tag in
      (EApp (f, args, tag), tag + 1)
  | ETuple (elements, _) ->
      let elements, tag = tag_args elements tag in
      (ETuple (elements, tag), tag + 1)
  | EGetItem (tuple, index, _) ->
      let tuple, tag = tag_expr tuple tag in
      let index, tag = tag_expr index tag in
      (EGetItem (tuple, index, tag), tag + 1)
  | ELambda (params, body, _) ->
      let body, tag = tag_expr body tag in
      (ELambda (params, body, tag), tag + 1)
  | ELetRec (bindings, body, _) ->
      let bindings, tag = tag_bindings bindings tag in
      let body, tag = tag_expr body tag in
      (ELetRec (bindings, body, tag), tag + 1)

and tag_bindings bindings tag =
  let bindings, tag =
    List.fold_left
      (fun (bindings, tag) (name, args, body, _) ->
        let body, tag = tag_expr body tag in
        ((name, args, body, tag) :: bindings, tag))
      ([], tag) bindings
  in
  (List.rev bindings, tag)

and tag_args args tag =
  let args, tag =
    List.fold_left
      (fun (args, tag) arg ->
        let arg, tag = tag_expr arg tag in
        (arg :: args, tag))
      ([], tag) args
  in
  (List.rev args, tag)

let tag p = tag_program p
