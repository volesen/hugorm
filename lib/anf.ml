open Syntax

type tag = int

type immexpr = ImmNum of int64 | ImmBool of bool | ImmId of string

and cexpr =
  | CPrim1 of prim1 * immexpr
  | CPrim2 of prim2 * immexpr * immexpr
  | CIf of immexpr * aexpr * aexpr
  | CImmExpr of immexpr

and aexpr = ALet of string * cexpr * aexpr | ACExpr of cexpr

let anf (e : tag expr) : aexpr =
  let rec anf e k =
    let ( let* ) = ( @@ ) in
    match e with
    | ENumber (n, _) -> k (ImmNum n)
    | EBool (b, _) -> k (ImmBool b)
    | EId (x, _) -> k (ImmId x)
    | EPrim1 (op, e, tag) ->
        let id = "prim1_" ^ string_of_int tag in
        let* imm = anf e in
        ALet (id, CImmExpr imm, ACExpr (CPrim1 (op, imm)))
    | EPrim2 (op, l, r, tag) ->
        let l_id = "prim2_l_" ^ string_of_int tag in
        let r_id = "prim2_r_" ^ string_of_int tag in
        let* l_imm = anf l in
        let* r_imm = anf r in
        ALet
          ( l_id,
            CImmExpr l_imm,
            ALet
              ( r_id,
                CImmExpr r_imm,
                ACExpr (CPrim2 (op, ImmId l_id, ImmId r_id)) ) )
    | EIf (cond, thn, els, _) ->
        let* cond_imm = anf cond in
        ACExpr (CIf (cond_imm, anf thn k, anf els k))
    | _ -> failwith ""
  in
  anf e (fun imm -> ACExpr (CImmExpr imm))

let if_test =
  EIf
    ( EPrim2 (Eq, ENumber (42L, 0), ENumber (69L, 0), 0),
      ENumber (1L, 0),
      ENumber (2L, 0),
      0 )
