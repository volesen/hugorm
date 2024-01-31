open Syntax

type 'a aprogram = { decls : 'a adecl list; body : 'a aexpr } [@@deriving show]
and 'a adecl = ADFun of string * string list * 'a aexpr * 'a

and 'a aexpr =
  | ALet of string * 'a cexpr * 'a aexpr * 'a
  | ALetRec of string * 'a cexpr * 'a aexpr * 'a
  | ACExpr of 'a cexpr

and 'a cexpr =
  | CIf of 'a immexpr * 'a aexpr * 'a aexpr * 'a
  | CPrim1 of prim1 * 'a immexpr * 'a
  | CPrim2 of prim2 * 'a immexpr * 'a immexpr * 'a
  | CApp of 'a immexpr * 'a immexpr list * 'a
  | CTuple of 'a immexpr list * 'a
  | CGetItem of 'a immexpr * 'a immexpr * 'a
  | CLambda of string list * 'a aexpr * 'a
  | CImmExpr of 'a immexpr

and 'a immexpr =
  | ImmNum of int64 * 'a
  | ImmBool of bool * 'a
  | ImmId of string * 'a

type ctx = (string * unit cexpr * bool) list


let rec anf (e : 'a expr) : unit aexpr =
  let ( let* ) = ( @@ ) in

  let rec map f lst k =
    match lst with
    | [] -> k []
    | x :: xs ->
        let* x = f x in
        let* xs = map f xs in
        k (x :: xs)
  in

  let rec anf_aexpr e k =
    match e with
    | ELet (x, e, body, _) ->
        let* cexpr = anf_cexpr e in
        k (ALet (x, cexpr, anf body, ()))
    | ELetRec (x, e, body, _) ->
        let* cexpr = anf_cexpr e in
        k (ALetRec (x, cexpr, anf body, ()))
    | _ ->
        let* cexpr = anf_cexpr e in
        k (ACExpr cexpr)
  and anf_cexpr e k =
    match e with
    | EPrim1 (op, e, _) ->
        let* imm = anf_immexpr e in
        k (CPrim1 (op, imm, ()))
    | EPrim2 (op, left, right, _) ->
        let* left_imm = anf_immexpr left in
        let* right_imm = anf_immexpr right in
        k (CPrim2 (op, left_imm, right_imm, ()))
    | EIf (cond, thn, els, _) ->
        let* cond_imm = anf_immexpr cond in
        k (CIf (cond_imm, anf thn, anf els, ()))
    | EApp (func, args, _) ->
        let* func_imm = anf_immexpr func in
        let* args_imms = map anf_immexpr args in
        k (CApp (func_imm, args_imms, ()))
    | ETuple (exprs, _) ->
        let* imms = map anf_immexpr exprs in
        k (CTuple (imms, ()))
    | EGetItem (expr, idx, _) ->
        let* expr_imm = anf_immexpr expr in
        let* idx_imm = anf_immexpr idx in
        k (CGetItem (expr_imm, idx_imm, ()))
    | ELambda (args, body, _) ->
        let aexpr = anf body in
        k (CLambda (args, aexpr, ()))
    | _ ->
        let* imm = anf_immexpr e in
        k (CImmExpr imm)
  and anf_immexpr e k =
    match e with
    | ENumber (n, _) -> k (ImmNum (n, ()))
    | EBool (b, _) -> k (ImmBool (b, ()))
    | EId (x, _) -> k (ImmId (x, ()))
    | _ ->
        let x = "tmp$" ^ string_of_int (tag_of_expr e) in
        let* cexpr = anf_cexpr e in
        ALet (x, cexpr, k (ImmId (x, ())), ())
  in
  anf_aexpr e Fun.id

let anf_decl (d : tag decl) : unit adecl =
  match d with
  | DFun (name, args, body, _) ->
      let body = anf body in
      ADFun (name, args, body, ())

let anf_program (p : tag program) : unit aprogram =
  let decls = List.map anf_decl p.decls in
  let body = anf p.body in
  { decls; body }

let tag (e : unit aprogram) : tag aprogram =
  let rec tag_adecl (d : unit adecl) (tag : tag) : tag adecl * tag =
    match d with
    | ADFun (name, args, body, ()) ->
        let body, tag = tag_aexpr body tag in
        (ADFun (name, args, body, tag), tag + 1)
  and tag_aexpr (e : unit aexpr) (tag : tag) : tag aexpr * tag =
    match e with
    | ALet (x, e, body, ()) ->
        let e, tag = tag_cexpr e tag in
        let body, tag = tag_aexpr body tag in
        (ALet (x, e, body, tag), tag + 1)
    | ALetRec (x, e, body, ()) ->
        let e, tag = tag_cexpr e tag in
        let body, tag = tag_aexpr body tag in
        (ALetRec (x, e, body, tag), tag + 1)
    | ACExpr cexpr ->
        let cexpr, tag = tag_cexpr cexpr tag in
        (ACExpr cexpr, tag + 1)
  and tag_cexpr (e : unit cexpr) (tag : tag) : tag cexpr * tag =
    match e with
    | CIf (cond, thn, els, ()) ->
        let cond, tag = tag_immexpr cond tag in
        let thn, tag = tag_aexpr thn tag in
        let els, tag = tag_aexpr els tag in
        (CIf (cond, thn, els, tag), tag + 1)
    | CPrim1 (op, e, ()) ->
        let e, tag = tag_immexpr e tag in
        (CPrim1 (op, e, tag), tag + 1)
    | CPrim2 (op, l, r, ()) ->
        let l, tag = tag_immexpr l tag in
        let r, tag = tag_immexpr r tag in
        (CPrim2 (op, l, r, tag), tag + 1)
    | CApp (f, args, ()) ->
        let f, tag = tag_immexpr f tag in
        let args, tag =
          List.fold_right
            (fun arg (args, tag) ->
              let arg, tag = tag_immexpr arg tag in
              (arg :: args, tag))
            args ([], tag)
        in
        (CApp (f, args, tag), tag + 1)
    | CTuple (elements, ()) ->
        let elements, tag =
          List.fold_right
            (fun element (elements, tag) ->
              let arg, tag = tag_immexpr element tag in
              (arg :: elements, tag))
            elements ([], tag)
        in
        (CTuple (elements, tag), tag + 1)
    | CGetItem (tuple, index, _) ->
        let tuple, tag = tag_immexpr tuple tag in
        let index, tag = tag_immexpr index tag in
        (CGetItem (tuple, index, tag), tag + 1)
    | CLambda (params, body, _) ->
        let body, tag = tag_aexpr body tag in
        (CLambda (params, body, tag), tag + 1)
    | CImmExpr e ->
        let e, tag = tag_immexpr e tag in
        (CImmExpr e, tag + 1)
  and tag_immexpr (e : unit immexpr) (tag : tag) : tag immexpr * tag =
    match e with
    | ImmNum (n, ()) -> (ImmNum (n, tag), tag + 1)
    | ImmBool (b, ()) -> (ImmBool (b, tag), tag + 1)
    | ImmId (x, ()) -> (ImmId (x, tag), tag + 1)
  in
  let decls, tag =
    List.fold_right
      (fun decl (decls, tag) ->
        let decl, tag = tag_adecl decl tag in
        (decl :: decls, tag))
      e.decls ([], 0)
  in
  let body, _ = tag_aexpr e.body tag in
  { decls; body }
