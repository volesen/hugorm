open Hugorm
open Hugorm.Asm
open Hugorm.Syntax
open Hugorm.Compile

let asm =
  let pp_asm fmt asm = Format.fprintf fmt "%s" (Asm.string_of_asm asm) in
  Alcotest.testable pp_asm ( = )

let test_compile_num () =
  Alcotest.(check asm)
    "Compile `Num`"
    [ IMov (Reg RAX, Const 84L) ]
    (Compile.compile_expr Env.empty 0 (ENumber (42L, 1)))

let test_compile_add1 () =
  Alcotest.(check asm)
    "Compile `Add1`"
    [ IMov (Reg RAX, Const 84L); INeg (Reg RAX) ]
    (Compile.compile_expr Env.empty  0 (EPrim1 (Neg, ENumber (42L, 1), 2)))

let test_count_let_imm () =
  Alcotest.(check int)
    "Test count let for immediate values" 0
    (count_let
       (to_anf (EPrim2 (Add, ENumber (42L, 1), ENumber (69L, 2), 3))))


let () =
  Alcotest.run "Compile"
    [
      ( "Compile expr",
        [
          Alcotest.test_case "Compile `Num`" `Quick test_compile_num;
          Alcotest.test_case "Compile `Add1`" `Quick test_compile_add1;
        ] );
      ("Count let", [ Alcotest.test_case "" `Quick test_count_let_imm ]);
    ]
