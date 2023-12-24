open Syntax

let parse (s : string) : unit program =
  s |> Lexing.from_string |> Parser.program Lexer.read

let parse_file (filename : string) : unit program =
  filename |> open_in |> Lexing.from_channel |> Parser.program Lexer.read
