open Printf
open Hugorm
open Hugorm.Syntax

let () =
  let input_program = Let ("x", Num 1L, Let ("x", Num 2L, Id "x")) in
  let program = Compile.compile_program input_program in
  printf "%s\n" program
