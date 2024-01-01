open Anf
module S = Set.Make (String)

let fvs (aexpr : 'a aexpr) : string list =
  let rec fvs_i imm =
    match imm with
    | ImmNum _ | ImmBool _ -> S.empty
    | ImmId (x, _) -> S.singleton x
  and fvs_c cexpr =
    match cexpr with
    | CPrim1 (_, imm, _) -> fvs_i imm
    | CPrim2 (_, imm1, imm2, _) -> S.union (fvs_i imm1) (fvs_i imm2)
    | CIf (imm, aexpr1, aexpr2, _) ->
        S.union (fvs_i imm) (S.union (fvs_a aexpr1) (fvs_a aexpr2))
    | CApp (imm, imms, _) ->
        List.fold_left (fun acc imm -> S.union acc (fvs_i imm)) (fvs_i imm) imms
    | CTuple (imms, _) ->
        List.fold_left (fun acc imm -> S.union acc (fvs_i imm)) S.empty imms
    | CGetItem (imm1, imm2, _) -> S.union (fvs_i imm1) (fvs_i imm2)
    | CLambda (xs, aexpr, _) -> S.diff (fvs_a aexpr) (S.of_list xs)
    | CImmExpr imm -> fvs_i imm
  and fvs_a aexpr =
    match aexpr with
    | ALet (x, cexpr, aexpr, _) ->
        S.union (fvs_c cexpr) (S.remove x (fvs_a aexpr))
    | ACExpr cexpr -> fvs_c cexpr
  in
  S.elements (fvs_a aexpr)
