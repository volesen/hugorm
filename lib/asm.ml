type reg64 =
  | RAX (* Return value *)
  | RBX
  | RCX
  | RDX
  | RDI
  | RSI
  | RBP (* Base pointer *)
  | RSP (* Stack pointer *)
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type arg =
  | Const of int64
  | Reg of reg64
  | RegOffset of reg64 * int (* E.g. [RBP-8] *)
  | RegBaseOffset of reg64 * reg64 * int * int (* E.g. [RAX+R11*8+8] *)
  | Label of string

type instruction =
  (* Directives *)
  | ISection of string
  | IGlobal of string
  | IExtern of string
  (* Data movement *)
  | IMov of arg * arg
  | ILea of arg * arg
  | IPush of arg
  | IPop of arg
  (* Arithmetic and logic *)
  | INeg of arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ISal of arg * arg
  | ISar of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  (* Control flow *)
  | ILabel of string
  | IJmp of string
  | IJe of string
  | IJne of string
  | IJl of string
  | IJg of string
  | IJle of string
  | IJge of string
  | IJz of string
  | IJnz of string
  (* Comparison *)
  | ICmp of arg * arg
  | ITest of arg * arg
  (* Function calling *)
  | ICall of arg
  | IRet

type asm = instruction list

let reg64_size_bytes = 8
let caller_saved_regs = [ RAX; RCX; RDX; RDI; RSI; RSP; R8; R9; R10; R11; R12 ]
let callee_saved_regs = [ RBX; RBP; R12; R13; R14; R15 ]
let arg_passing_regs = [ RDI; RSI; RDX; RCX; R8; R9 ]

let string_of_reg (reg : reg64) : string =
  match reg with
  | RAX -> "RAX"
  | RBX -> "RBX"
  | RCX -> "RCX"
  | RDX -> "RDX"
  | RDI -> "RDI"
  | RSI -> "RSI"
  | RBP -> "RBP"
  | RSP -> "RSP"
  | R8 -> "R8"
  | R9 -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"
  | R12 -> "R12"
  | R13 -> "R13"
  | R14 -> "R14"
  | R15 -> "R15"

let string_of_arg (arg : arg) : string =
  match arg with
  | Const int -> Int64.to_string int
  | Reg reg -> string_of_reg reg
  | RegOffset (reg, offset) ->
      Format.sprintf "[%s%+d]" (string_of_reg reg) offset
  | RegBaseOffset (reg1, reg2, scale, offset) ->
      Format.sprintf "[%s+%s*%d%+d]" (string_of_reg reg1) (string_of_reg reg2)
        scale offset
  | Label label -> label

let string_of_instruction (instr : instruction) : string =
  let string_of_unary_op op arg =
    Format.sprintf "  %s %s" op (string_of_arg arg)
  in
  let string_of_binary_op op arg1 arg2 =
    Format.sprintf "  %s %s, %s" op (string_of_arg arg1) (string_of_arg arg2)
  in
  match instr with
  | ISection section -> "section ." ^ section
  | IGlobal label -> "  global " ^ label
  | IExtern label -> "  extern " ^ label
  | IMov (arg1, arg2) -> string_of_binary_op "mov" arg1 arg2
  | ILea (arg1, arg2) ->
      Format.sprintf "  lea %s, [rel %s]" (string_of_arg arg1)
        (string_of_arg arg2)
  | IPush arg -> string_of_unary_op "push" arg
  | IPop arg -> string_of_unary_op "pop" arg
  | INeg arg -> string_of_unary_op "neg" arg
  | IAdd (arg1, arg2) -> string_of_binary_op "add" arg1 arg2
  | ISub (arg1, arg2) -> string_of_binary_op "sub" arg1 arg2
  | IMul (arg1, arg2) -> string_of_binary_op "imul" arg1 arg2
  | ISal (arg1, arg2) -> string_of_binary_op "sal" arg1 arg2
  | ISar (arg1, arg2) -> string_of_binary_op "sar" arg1 arg2
  | IAnd (arg1, arg2) -> string_of_binary_op "and" arg1 arg2
  | IOr (arg1, arg2) -> string_of_binary_op "or" arg1 arg2
  | IXor (arg1, arg2) -> string_of_binary_op "xor" arg1 arg2
  | ILabel label -> label ^ ":"
  | IJmp label -> "  jmp " ^ label
  | IJe label -> "  je " ^ label
  | IJne label -> "  jne " ^ label
  | IJl label -> "  jl " ^ label
  | IJg label -> "  jg " ^ label
  | IJle label -> "  jle " ^ label
  | IJge label -> "  jge " ^ label
  | IJz label -> "  jz " ^ label
  | IJnz label -> "  jnz " ^ label
  | ICmp (arg1, arg2) -> string_of_binary_op "cmp" arg1 arg2
  | ITest (arg1, arg2) -> string_of_binary_op "test" arg1 arg2
  | ICall arg -> string_of_unary_op "call" arg
  | IRet -> "  ret"

let string_of_asm (asm : asm) : string =
  asm |> List.map string_of_instruction |> String.concat "\n"
