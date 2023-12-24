{
open Lexing
open Parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let id = alpha (alpha | digit | '_')*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int      { INT (Int64.of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "let"    { LET }
  | "in"     { IN }
  | "def"    { DEF }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { STAR }
  | '='      { EQ }
  | '<'      { LT }
  | "<="     { LTE }
  | '>'      { GT }
  | ">="     { GTE }
  | ','      { COMMA }
  | ':'      { COLON }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | id       { ID (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
