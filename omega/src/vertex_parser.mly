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
module V = Vertex_syntax
module E = Vertex_syntax.Expr
module M = Vertex_syntax.Model
let parse_error msg =
  raise (V.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < int > DIGIT
%token < string > NAME
%token < string > BEGIN_ENV END_ENV
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE LANGLE RANGLE
%token SUPER SUB COMMA VERT EQUAL
%token PLUS MINUS TIMES DIV DOT TILDE
%token SEP END

%token PARTICLE PARAMETER LAGRANGIAN
%token START STOP TEX

%left PLUS MINUS
%nonassoc NEG UPLUS TILDE
%left TIMES
%left DIV
%left DOT

%start model
%type < Vertex_syntax.Model.t > model

%%

model:
 | declarations END { $1 }
;

declarations:
 |                                 { M.empty }
 | particle declarations           { $2 }
 | parameter declarations          { $2 }
 | set_lagrangian declarations     { M.l $1 $2 }
 | augment_lagrangian declarations { M.l $1 $2 }
;

particle:
 | PARTICLE TEX LBRACE token_list RBRACE  { M.empty }
;

parameter:
 | PARAMETER TEX LBRACE token_list RBRACE { M.empty }
;

set_lagrangian:
 | LAGRANGIAN EQUAL expr vertex     { ($3, $4) }
 | LAGRANGIAN EQUAL vertex          { (E.integer 1, $3) }
;

augment_lagrangian:
 | LAGRANGIAN PLUS EQUAL expr vertex     { ($4, $5) }
 | LAGRANGIAN PLUS EQUAL vertex          { (E.integer 1, $4) }
;

expr:
 | integer                 { E.integer $1 }
 | LPAREN expr RPAREN      { $2 }
 | expr PLUS expr          { E.add $1 $3 }
 | expr MINUS expr         { E.sub $1 $3 }
 | expr TIMES expr         { E.mult $1 $3 }
 | expr DIV expr           { E.div $1 $3 }
 | NAME arg_list           { E.apply $1 $2 }
;

arg_list:
 |                         { [] }
 | arg arg_list            { $1 :: $2 }
;

arg:
 | LBRACE expr RBRACE      { $2 }
;


integer:
 | DIGIT           { $1 }
 | integer DIGIT   { 10 * $1 + $2 }
;

vertex:
 | START STOP                { V.List [] }
 | START token_list STOP     { V.List $2 }
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
     { V.Scripted { V.token = $1; V.super = []; V.sub = [] } }
 | token SUPER token
     { V.Scripted { V.token = $1; V.super = V.plug $3; V.sub = [] } }
 | token SUB token
     { V.Scripted { V.token = $1; V.super = []; V.sub = V.plug $3 } }
 | token SUPER token SUB token
     { V.Scripted { V.token = $1; V.super = V.plug $3; V.sub = V.plug $5 } }
 | token SUB token SUPER token
     { V.Scripted { V.token = $1; V.super = V.plug $5; V.sub = V.plug $3 } }
;

token:
 | bare_token
     { $1 }
 | LBRACE token RBRACE
     { $2 }
 | LBRACE token token_list RBRACE
     { V.List ($2 :: $3) }
;

bare_token:
 | DIGIT { V.Digit $1 }
 | NAME  { V.Name $1 }
;

