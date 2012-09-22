(* $Id: vertex_lexer.mll 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let char = upper | lower
let white = [' ' '\t' '\n']

rule token = parse
    white      { token lexbuf }     (* skip blanks *)
  | '%' [^'\n']* '\n'
               { token lexbuf }     (* skip comments *)
  | '.'        { DOT }
  | '^'        { POWER }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '('        { LPAREN }
  | ','        { COMMA }
  | ')'        { RPAREN }
  | '<'        { BRA }
  | '|'        { VERT }
  | '>'        { KET }
  | '['        { LEXT }
  | ']'        { REXT }
  | digit+     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | 'e' digit+ { POLARIZATION (int_of_string (string_tail (Lexing.lexeme lexbuf))) }
  | 'k' digit+ { MOMENTUM (int_of_string (string_tail (Lexing.lexeme lexbuf))) }
  | 'i'        { I }
  | 'S'        { S }
  | 'P'        { P }
  | 'V'        { V }
  | 'A'        { A }
  | 'T'        { T }
  | "eps"      { EPSILON }
  | char (char|digit)*
               { NAME (Lexing.lexeme lexbuf) }
  | _          { failwith ("invalid character at `" ^ Lexing.lexeme lexbuf ^ "'") }
  | eof        { END }


