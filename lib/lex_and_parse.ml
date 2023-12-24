open Syntax

let parse (s : string) : unit expr =
  s |> Lexing.from_string |> Parser.program Lexer.read

let parse_file (filename : string) : unit expr =
  filename |> open_in |> Lexing.from_channel |> Parser.program Lexer.read
