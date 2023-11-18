open Asm
open Syntax

let compile_expr (n : expr) : asm = [ IMov (Reg RAX, Const n) ]

let compile_program (program : program) : string =
  let instrs = compile_expr program in
  let asm_string = string_of_asm instrs in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
