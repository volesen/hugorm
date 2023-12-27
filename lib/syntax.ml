type 'a program = { decls : 'a decl list; body : 'a expr } [@@deriving show]
and 'a decl = DFun of string * string list * 'a expr * 'a [@@deriving show]

and 'a expr =
  | ENumber of int64 * 'a
  | EBool of bool * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EId of string * 'a
  | ELet of string * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | EApp of string * 'a expr list * 'a
[@@deriving show]

and prim1 = Neg | Not [@@deriving show]

and prim2 =
  | Add
  | Sub
  | Mul
  | And
  | Or
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | Eq
  | Ne
[@@deriving show]

type tag = int [@@deriving show]

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
