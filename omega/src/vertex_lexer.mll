(* $Id: vertex_lexer.mll 4015 2013-01-03 16:04:18Z jr_reuter $

   Copyright (C) 1999-2013 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <cnspeckn@googlemail.com>

   WHIZARD is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   WHIZARD is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

{
open Vertex_parser
let string_tail s =
  String.sub s 1 (String.length s - 1)
let string_trunc s =
  String.sub s 1 (String.length s - 2)
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let char = upper | lower
let white = [' ' '\t' '\n']

rule token names = parse
    white      { token names lexbuf }     (* skip blanks *)
  | '#' [^'\n']* '\n'
               { token names lexbuf }     (* skip comments *)
  | '~'        { TILDE }
  | '.'        { DOT }
  | '^'        { SUPER }
  | '_'        { SUB }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | '['        { LBRACKET }
  | ']'        { RBRACKET }
  | '<'        { LANGLE }
  | '>'        { RANGLE }
  | ','        { COMMA }
  | '|'        { VERT }
  | digit+     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | 'I'        { I }
  | '@' digit+ { let tail = string_tail (Lexing.lexeme lexbuf) in
                 MOMENTUM (int_of_string tail) }
  | char (char|digit)*
               { let s = Lexing.lexeme lexbuf in
                 match names.Vertex_syntax.identifier s with
                 | Vertex_syntax.Id_Color -> COLOR s
                 | Vertex_syntax.Id_Lorentz -> LORENTZ s
                 | _ -> NAME s }
  | '"' [^'"']* '"'
               { NAME (string_trunc (Lexing.lexeme lexbuf)) }
  | _          { failwith ("invalid character at `" ^
			      Lexing.lexeme lexbuf ^ "'") }
  | eof        { END }


