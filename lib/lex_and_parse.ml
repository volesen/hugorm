let parse (s : string) = s |> Lexing.from_string |> Parser.program Lexer.read

let parse_file (filename : string) =
  filename |> open_in |> Lexing.from_channel |> Parser.program Lexer.read
