open Printf
open Hugorm.Main
open Hugorm.Syntax

let () =
  let input_program = Add1 (Num 41L) in
  let program = compile_program input_program in
  printf "%s\n" program
