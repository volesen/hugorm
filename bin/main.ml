open Printf
open Hugorm
open Hugorm.Syntax

let () =
    (*
    let input_program =
    ELet
      ( "x",
        ENumber (1L, ""),
        ELet ("x", ENumber (2L, ""), EId ("x", ""), ""),
        "" )
  in
  *)
  let if_program = 
    ELet(
      "x",
      EPrim1(Sub1, ENumber(2L, ""), ""),
      EIf(
        EId("x", ""),
        ENumber(42L, ""),
        ENumber(69L, ""),
        ""
      ),
      ""
    )
  in
  let program = Compile.compile_program if_program in
  printf "%s\n" program
