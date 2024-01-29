open Syntax
(** Well-formedness checking for the AST.

    The following checks are performed:
    - Integer literals are in the range [-2^62, 2^62 - 1]
    - Variables are bound before use
    - Lambda parameters are distinct
    - The right-hand side of a letrec is a lambda (enforced by the parser)
*)

module SS = Set.Make (String)

type loc = Lexing.position

exception Unbound of string * loc
exception Integer_overflow of loc
exception Duplicate_param of string * loc
exception Letrec_non_function of string * loc

let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L

(** [well_formed e] checks that [e] is well-formed according to the
    criteria above.  If [e] is not well-formed, it raises an exception
    indicating which criterion was violated. *)
let well_formed e =
  let rec well_formed env e =
    match e with
    | ENumber (n, loc) ->
        if n > max_int || n < min_int then raise (Integer_overflow loc)
    | EBool _ -> ()
    | EId (x, loc) -> if not (SS.mem x env) then raise (Unbound (x, loc))
    | EPrim1 (_, e, _) -> well_formed env e
    | EPrim2 (_, l, r, _) ->
        well_formed env l;
        well_formed env r
    | ELet (x, b, body, _) ->
        well_formed env b;
        well_formed (SS.add x env) body
    | ELetRec (x, b, body, loc) ->
        let check_lambda e =
          match e with
          | ELambda _ -> ()
          | _ -> raise (Letrec_non_function (x, loc))
        in
        check_lambda b;
        well_formed (SS.add x env) b;
        well_formed (SS.add x env) body
    | EIf (c, t, f, _) ->
        well_formed env c;
        well_formed env t;
        well_formed env f
    | EApp (f, args, _) ->
        well_formed env f;
        List.iter (well_formed env) args
    | ETuple (exprs, _) -> List.iter (well_formed env) exprs
    | EGetItem (e, _, _) -> well_formed env e
    | ELambda (xs, body, loc) ->
        let rec check_dup xs =
          match xs with
          | [] -> ()
          | x :: xs ->
              if List.mem x xs then raise (Duplicate_param (x, loc))
              else check_dup xs
        in
        check_dup xs;
        well_formed (SS.union (SS.of_list xs) env) body
  in

  well_formed SS.empty e
