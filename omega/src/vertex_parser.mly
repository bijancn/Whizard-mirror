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
let parse_error msg =
  raise (Vertex_syntax.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < int > INT
%token < string > NAME
%token < int > MOMENTUM
%token PARTIAL
%token EPSILON
%token S V T A P
%token I
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE LANGLE RANGLE
%token SUPER SUB COMMA VERT
%token PLUS MINUS TIMES DIV DOT
%token END

%left PLUS MINUS
%nonassoc NEG UPLUS
%left TIMES
%left DIV
%left DOT

%start vertex
%type < Vertex_syntax.t > vertex

%%

vertex:
 | factor                      { Vertex_syntax.null }
 | factor vertex               { Vertex_syntax.null }
 | END                         { Vertex_syntax.null }
;

factor:
 | spinor_current              { () }
 | vector                      { () }
;

spinor_current:
 | LANGLE conjspinor VERT dirac VERT spinor RANGLE
                               { () }
;

vector:
 | NAME LBRACKET NAME RBRACKET { () }
;

spinor:
 | NAME                        { () }
;

conjspinor:
 | NAME                        { () }
;

dirac:
 | NAME LBRACKET NAME RBRACKET { () }
;
