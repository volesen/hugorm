type reg = RAX
type arg = Const of int64 | Reg of reg
type instruction = IMov of arg * arg | IAdd of arg * arg
type asm = instruction list

let string_of_reg (reg : reg) : string = match reg with RAX -> "RAX"

let string_of_arg (arg : arg) : string =
  match arg with
  | Const int -> Int64.to_string int
  | Reg reg -> string_of_reg reg

let string_of_instruction (instr : instruction) : string =
  match instr with
  | IMov (arg1, arg2) ->
      "  mov " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2
  | IAdd (arg1, arg2) ->
      "  add " ^ string_of_arg arg1 ^ ", " ^ string_of_arg arg2

let string_of_asm (asm : asm) : string =
  asm |> List.map string_of_instruction |> String.concat "\n"
