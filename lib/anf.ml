open Syntax

type 'a aprogram = { decls : 'a adecl list; body : 'a aexpr }
and 'a adecl = ADFun of string * string list * 'a aexpr * 'a
and 'a aexpr = ALet of string * 'a cexpr * 'a aexpr * 'a | ACExpr of 'a cexpr

and 'a cexpr =
  | CIf of 'a immexpr * 'a aexpr * 'a aexpr * 'a
  | CPrim1 of prim1 * 'a immexpr * 'a
  | CPrim2 of prim2 * 'a immexpr * 'a immexpr * 'a
  | CApp of string * 'a immexpr list * 'a
  | CPair of 'a immexpr * 'a immexpr * 'a
  | CImmExpr of 'a immexpr

and 'a immexpr =
  | ImmNil of 'a
  | ImmNum of int64 * 'a
  | ImmBool of bool * 'a
  | ImmId of string * 'a

type ctx = (string * unit cexpr) list

let anf (e : tag expr) : unit aexpr =
  let rec help_a (e : tag expr) : unit aexpr =
    let cexpr, bindings = help_c e in
    (* Enclose `cexpr` in nested let-bindings *)
    List.fold_right
      (fun (x, e) body -> ALet (x, e, body, ()))
      bindings (ACExpr cexpr)
  and help_c (e : tag expr) : unit cexpr * ctx =
    match e with
    | (ENumber _ | EBool _ | EId _ | ENil _) as e ->
        let imm, ctx = help_i e in
        (CImmExpr imm, ctx)
    | EPrim1 (op, e, _) ->
        let imm, ctx = help_i e in
        (CPrim1 (op, imm, ()), ctx)
    | EPrim2 (op, l, r, _) ->
        let l_imm, l_ctx = help_i l in
        let r_imm, r_ctx = help_i r in
        (CPrim2 (op, l_imm, r_imm, ()), l_ctx @ r_ctx)
    | EIf (cond, thn, els, _) ->
        let cond_imm, cond_ctx = help_i cond in
        let thn_aexpr = help_a thn in
        let els_aexpr = help_a els in
        (CIf (cond_imm, thn_aexpr, els_aexpr, ()), cond_ctx)
    | ELet (x, e, body, _) ->
        let e_cexpr, e_ctx = help_c e in
        let body_cexpr, body_ctx = help_c body in
        (body_cexpr, e_ctx @ [ (x, e_cexpr) ] @ body_ctx)
    | EApp (f, args, _) ->
        let arg_imms, ctxs = args |> List.map help_i |> List.split in
        let ctx = List.concat ctxs in
        (CApp (f, arg_imms, ()), ctx)
    | EPair (fst, snd, _) ->
        let fst_imm, fst_ctx = help_i fst in
        let snd_imm, snd_ctx = help_i snd in
        (CPair (fst_imm, snd_imm, ()), fst_ctx @ snd_ctx)
  and help_i (e : tag expr) : unit immexpr * ctx =
    match e with
    | ENil _ -> (ImmNil (), [])
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (x, _) -> (ImmId (x, ()), [])
    | (EPrim1 _ | EPrim2 _ | EIf _ | ELet _ | EApp _ | EPair _) as e ->
        imm_of_cexpr e
  and imm_of_cexpr (e : tag expr) : unit immexpr * ctx =
    let tag = tag_of_expr e in
    let x = "x" ^ string_of_int tag in
    let cexpr, ctx = help_c e in
    (ImmId (x, ()), ctx @ [ (x, cexpr) ])
  in
  help_a e

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
        let args, tag =
          List.fold_right
            (fun arg (args, tag) ->
              let arg, tag = tag_immexpr arg tag in
              (arg :: args, tag))
            args ([], tag)
        in
        (CApp (f, args, tag), tag + 1)
    | CPair (fst, snd, ()) ->
        let fst, tag = tag_immexpr fst tag in
        let snd, tag = tag_immexpr snd tag in
        (CPair (fst, snd, tag), tag + 1)
    | CImmExpr e ->
        let e, tag = tag_immexpr e tag in
        (CImmExpr e, tag + 1)
  and tag_immexpr (e : unit immexpr) (tag : tag) : tag immexpr * tag =
    match e with
    | ImmNil _ -> (ImmNil tag, tag + 1)
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
