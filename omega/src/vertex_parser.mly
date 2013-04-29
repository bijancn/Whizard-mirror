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

%token < int > DIGIT
%token < string > NAME
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
%type < Vertex_syntax.token > vertex

%%

vertex:
 | token_list END  { S.List $1 }
 | END             { S.List [] }
;

token_list:
 | scripted_token            { [$1] }
 | scripted_token token_list { $1 :: $2 }
 /* Right recursion is more convenient for constructing
    the value.  Since the lists will always be short,
    there is no performace or stack size reason for
    prefering left recursion. */
;

scripted_token:
 | token
     { S.Scripted { S.token = $1; S.super = []; S.sub = [] } }
 | token SUPER token
     { S.Scripted { S.token = $1; S.super = S.plug $3; S.sub = [] } }
 | token SUB token
     { S.Scripted { S.token = $1; S.super = []; S.sub = S.plug $3 } }
 | token SUPER token SUB token
     { S.Scripted { S.token = $1; S.super = S.plug $3; S.sub = S.plug $5 } }
 | token SUB token SUPER token
     { S.Scripted { S.token = $1; S.super = S.plug $3; S.sub = S.plug $5 } }
;

token:
 | bare_token
     { $1 }
 | LBRACE token RBRACE
     { $2 }
 | LBRACE token token_list RBRACE
     { S.List ($2 :: $3) }
;

bare_token:
 | DIGIT { S.Digit $1 }
 | NAME  { S.Name $1 }
;

