open Syntax

let parse (s : string) : unit program =
  s |> Lexing.from_string |> Parser.program Lexer.read
