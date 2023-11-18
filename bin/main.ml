open Printf
open Hugorm.Main

let () =
  let input_file = open_in Sys.argv.(1) in
  let input_program = Int64.of_string (input_line input_file) in
  close_in input_file;
  let program = compile input_program in
  printf "%s\n" program
