/* $Id: vertex_parser.mly 4015 2013-01-03 16:04:18Z jr_reuter $

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

%{
module S = Vertex_syntax
let parse_error msg =
  raise (S.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < int > INT
%token < string > NAME
%token < string > LORENTZ
%token < string > COLOR
%token < int > MOMENTUM
%token EPSILON
%token S V T A P
%token I
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE LANGLE RANGLE
%token SUPER SUB COMMA VERT
%token PLUS MINUS TIMES DIV DOT TILDE
%token END

%left PLUS MINUS
%nonassoc NEG UPLUS TILDE
%left TIMES
%left DIV
%left DOT

%start vertex
%type < Vertex_syntax.t > vertex

%%

vertex:
 | term              { $1 }
 | vertex PLUS term  { let t = match $1 with S.Sum s -> s | t -> [(1, t)] in
		       S.Sum (t @ [( 1, $3)]) }
 | vertex MINUS term { let t = match $1 with S.Sum s -> s | t -> [(1, t)] in
		       S.Sum (t @ [(-1, $3)]) }
 | vertex END        { $1 }
 | END               { S.null }
;

term:
 | factor       { $1 }
 | factor term  { let t = match $2 with S.Product p -> p | t -> [t] in
		  S.Product ($1 :: t) }
;

factor:
 | lorentz                   { $1 }
 | color                     { $1 }
 | momentum                  { $1 }
 | field                     { $1 }
;

/*
coeff:
 | INT                       { $1 }
 | coeff TIMES coeff         { $1 * $3 }
 | coeff DIV coeff           { $1 / $3 }
;
*/

lorentz:
 | LORENTZ                       { S.Lorentz { S.t_name = $1;
					       S.t_indices = [] } }
 | LORENTZ LBRACE indices RBRACE { S.Lorentz { S.t_name = $1;
					       S.t_indices = $3 } }
;

color:
 | COLOR                         { S.Color { S.t_name = $1;
					     S.t_indices = [] } }
 | COLOR LBRACE indices RBRACE   { S.Color { S.t_name = $1;
					     S.t_indices = $3 } }
;

momentum:
 | momentum_sum LBRACE index RBRACE   { S.Momentum ($1, $3) }
;

momentum_sum:
 | MOMENTUM                      { [$1] }
 | MOMENTUM PLUS momentum_sum    { $1 :: $3 }
 /* Right recursion is more convenient for constructing
    the value.  Since the lists will always be short,
    there is no performace or stack size reason for
    prefering left recursion. */
;

field:
 | flavor                       { S.Field { S.flavor = fst $1;
					    S.conjugate = snd $1;
					    S.f_indices = [] } }
 | flavor LBRACE indices RBRACE { S.Field { S.flavor = fst $1;
					    S.conjugate = snd $1;
					    S.f_indices = $3 } }
;

flavor:
 | NAME       { ($1, false) }
 | TILDE NAME { ($2, true) }
;

indices:
 | index                      { [$1] }
 | index COMMA indices        { $1 :: $3 }
 /* Right recursion is more convenient for constructing
    the value.  Since the lists will always be short,
    there is no performace or stack size reason for
    prefering left recursion. */
;

index:
 | NAME                       { $1 }
;
