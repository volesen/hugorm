open Printf
open Hugorm
open Hugorm.Syntax

let expr =
  EPrim2
    (* 12 *)
    ( Add,
      EPrim1 (Neg, ENumber (5L, ()), ()),
      (* 4 *)
      EPrim2
        ( Mul,
          (* 8 *)
          ENumber (2L, ()),
          (* 4 *)
          EPrim1 (Neg, ENumber (3L, ()), ()),
          () ),
      (* 4 *)
      () )

let () =
  let program = Compile.compile_to_asm_string expr in
  printf "%s\n" program
