type reg = RAX | RSP

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* [reg + 8 * i] *)

type instruction =
  | IMov of arg * arg
  | INeg of arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ILabel of string
  | IJmp of string
  | IJe of string
  | ICmp of arg * arg
  | IRet

type asm = instruction list

let string_of_reg (reg : reg) : string =
  match reg with RAX -> "RAX" | RSP -> "RSP"

let string_of_arg (arg : arg) : string =
  match arg with
  | Const int -> Int64.to_string int
  | Reg reg -> string_of_reg reg
  | RegOffset (reg, offset) ->
      "[" ^ string_of_reg reg
      ^ (if offset >= 0 then "+" else "-")
      ^ string_of_int (8 * abs offset)
      ^ "]"

let string_of_instruction (instr : instruction) : string =
  match instr with
  | IMov (arg1, arg2) ->
      "  mov " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2
  | INeg arg -> "  neg " ^ string_of_arg arg
  | IAdd (arg1, arg2) ->
      "  add " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2
  | ISub (arg1, arg2) ->
      "  sub " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2
  | IMul (arg1, arg2) ->
      "  imul " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2
  | ILabel label -> label ^ ":"
  | IJmp label -> "  jmp " ^ label
  | IJe label -> "  je " ^ label
  | ICmp (arg1, arg2) ->
      "  cmp " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2
  | IRet -> "  ret"

let string_of_asm (asm : asm) : string =
  asm |> List.map string_of_instruction |> String.concat "\n"
