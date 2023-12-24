open Printf
open Hugorm

let () =
  (* Quick and dirty*)
  let input_filename = Sys.argv.(1) in
  let ast = Lex_and_parse.parse_file input_filename in
  let asm_string = Compile.compile_to_asm_string ast in

  (* Write the assembler to a file *)
  let oc = open_out "out/a.s" in
  fprintf oc "%s" asm_string;
  close_out oc;

  (* Assemble out.s to out.o *)
  let _ = Sys.command "nasm -f macho64 -o out/a.o out/a.s" in

  (* Link out.o to out *)
  let _ = Sys.command "clang -g -arch x86_64 -o out/a main.c out/a.o" in

  (* Run the program *)
  let _ = Sys.command "./out/a" in
  ()
