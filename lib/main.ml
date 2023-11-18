open Printf

let compile (program : int64) : string = 
  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, %Ld
  ret
" program

