type 'a program = 'a expr

and 'a expr =
  | ENumber of int64 * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EId of string * 'a
  | ELet of string * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a

and prim1 = Add1 | Sub1
