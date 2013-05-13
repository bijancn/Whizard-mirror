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
let string_of_char c = String.make 1 c
let int_of_char c = int_of_string (string_of_char c)
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let char = upper | lower
let white = [' ' '\t' '\n']
let pfx = '\\'

rule token = parse
    white             { token lexbuf }     (* skip blanks *)
  | '%' [^'\n']* '\n' { token lexbuf }     (* skip comments *)
  | '\\' ( [','';'] | 'q'? "quad" )
                      { token lexbuf }     (* skip LaTeX white space *)
  | "\\\\"            { token lexbuf }     (* skip table line breaks *)
  | '&'               { token lexbuf }     (* skip tabulators *)
  | '='        	      { EQUAL }
  | '^'        	      { SUPER }
  | '_'        	      { SUB }
  | '\''      	      { PRIME }
  | '*'        	      { TIMES }
  | '/'        	      { DIV }
  | '+'        	      { PLUS }
  | '-'        	      { MINUS }
  | ','        	      { COMMA }
  | '('        	      { LPAREN }
  | ')'        	      { RPAREN }
  | '{'        	      { LBRACE }
  | '}'        	      { RBRACE }
  | '['        	      { LBRACKET }
  | ']'        	      { RBRACKET }
  | pfx "include{" ([^'}']+ as name) "}"
                      { INCLUDE name }
  | pfx "charged"     { CHARGED }
  | pfx "neutral"     { NEUTRAL }
  | pfx "anti"        { ANTI }
  | pfx "TeX"         { TEX }
  | pfx "fortran"     { FORTRAN }
  | pfx "spin"        { SPIN }
  | pfx "color"       { COLOR }
  | pfx "charge"      { CHARGE }
  | pfx "vertex"      { VERTEX }
  | pfx "index"       { INDEX }
  | pfx "tensor"      { TENSOR }
  | pfx "lorentz"     { LORENTZ }
  | pfx "flavor"      { FLAVOR }
  | pfx "parameter"   { INPUT }
  | pfx "derived"     { DERIVED }
  | digit as i        { DIGIT (int_of_char i) }
  | char as c         { CHAR (string_of_char c) }
  | ('\\' (_ | char+)) as s
                      { TOKEN s }
  | _ as c            { failwith ("invalid character at `" ^
				     string_of_char c ^ "'") }
  | eof               { END }


