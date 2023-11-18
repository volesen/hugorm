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
    [ IMov (Reg RAX, Const 42L) ]
    (Compile.compile_expr Env.empty (Num 42L))

let test_compile_add1 () =
  Alcotest.(check asm)
    "Compile `Add1`"
    [ IMov (Reg RAX, Const 42L); IAdd (Reg RAX, Const 1L) ]
    (Compile.compile_expr Env.empty (Add1 (Num 42L)))

let () =
  Alcotest.run "Compile"
    [
      ( "Compile expr",
        [
          Alcotest.test_case "Compile `Num`" `Quick test_compile_num;
          Alcotest.test_case "Compile `Add1`" `Quick test_compile_add1;
        ] );
    ]
