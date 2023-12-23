open Printf
open Hugorm
open Hugorm.Syntax

let () =
  (*let expr = EBool (true, ()) in *)
  (*let expr = ENumber (3L, ()) in*)
  (*let expr = EPrim2 (Sub, ENumber (2L, ()), ENumber (1L, ()), ()) in*)
  (*let expr = EPrim2 (And, EBool(false, ()), EBool(true, ()), ()) in*)
  (*let expr = EPrim1 (Not, EBool(false, ()), ()) in*)
  let expr =
    EIf
      ( EPrim1
          ( Neg,
            EPrim1
              ( Print,
                EPrim2 (Greater, ENumber (42L, ()), ENumber (69L, ()), ()),
                () ),
            () ),
        ENumber (24L, ()),
        ENumber (96L, ()),
        () )
  in
  let program = Compile.compile_to_asm_string expr in
  (* Write the assembler to a file *)
  let oc = open_out "out/a.s" in
  fprintf oc "%s" program;
  close_out oc;

  (* Assemble out.s to out.o *)
  let _ = Sys.command "nasm -f macho64 -o out/a.o out/a.s" in

  (* Link out.o to out *)
  let _ = Sys.command "clang -g -arch x86_64 -o out/a main.c out/a.o" in

  (* Run the program *)
  let _ = Sys.command "./out/a" in
  ()
