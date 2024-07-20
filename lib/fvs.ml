open Anf
module S = Set.Make (String)

let rec fvs_immexpr imm =
  match imm with
  | ImmNum _ | ImmBool _ -> S.empty
  | ImmId (x, _) -> S.singleton x

and fvs_cexpr cexpr =
  match cexpr with
  | CPrim1 (_, imm, _) -> fvs_immexpr imm
  | CPrim2 (_, imm1, imm2, _) -> S.union (fvs_immexpr imm1) (fvs_immexpr imm2)
  | CIf (imm, aexpr1, aexpr2, _) ->
      S.union (fvs_immexpr imm) (S.union (fvs_aexpr aexpr1) (fvs_aexpr aexpr2))
  | CApp (imm, imms, _) ->
      List.fold_left
        (fun acc imm -> S.union acc (fvs_immexpr imm))
        (fvs_immexpr imm) imms
  | CTuple (imms, _) ->
      List.fold_left (fun acc imm -> S.union acc (fvs_immexpr imm)) S.empty imms
  | CGetItem (imm1, imm2, _) -> S.union (fvs_immexpr imm1) (fvs_immexpr imm2)
  | CLambda (params, body, _) -> S.diff (fvs_aexpr body) (S.of_list params)
  | CImmExpr imm -> fvs_immexpr imm

and fvs_aexpr aexpr =
  match aexpr with
  | ALet (x, cexpr, aexpr, _) ->
      S.union (fvs_cexpr cexpr) (S.remove x (fvs_aexpr aexpr))
  | ALetRec (bindings, aexpr, _) ->
      let xs = List.map (fun (x, _, _, _) -> x) bindings in
      let bindings_fvs =
        List.fold_left
          (fun acc (_, params, body, _) ->
            S.union acc (S.diff (fvs_aexpr body) (S.of_list params)))
          S.empty bindings
      in
      S.diff (S.union bindings_fvs (fvs_aexpr aexpr)) (S.of_list xs)
  | ACExpr cexpr -> fvs_cexpr cexpr
