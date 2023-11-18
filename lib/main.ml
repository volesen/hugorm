open Asm
open Syntax

let rec compile_expr (expr : expr) : asm =
  match expr with
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Add1 e -> compile_expr e @ [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 e -> compile_expr e @ [ IAdd (Reg RAX, Const (-1L)) ]

let compile_program (program : program) : string =
  let instrs = compile_expr program in
  let asm_string = string_of_asm instrs in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
