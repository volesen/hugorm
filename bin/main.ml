open Hugorm

let compile_to_asm (filename : string) =
  filename
  |> Hugorm.Lex_and_parse.parse_file
  |> Compile.compile
  |> Asm.string_of_asm
  |> print_endline

let usage_msg = "hugorm <filename>"

let filename = ref ""

let speclist = [
  ("-f", Arg.Set_string filename, ": specify input file")
]

let () = Arg.parse speclist compile_to_asm usage_msg
