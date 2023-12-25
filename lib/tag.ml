open Syntax

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
  | EApp (name, args, _) ->
      let args, tag = tag_args args tag in
      (EApp (name, args, tag), tag + 1)

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
