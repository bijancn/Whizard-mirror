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
%token SUPER SUB LBRACE RBRACE
%token LPAREN RPAREN
%token COMMA
%token PLUS MINUS TIMES DIV EQUAL
%token END

%token NEUTRAL CHARGED
%token ANTI ALIAS TEX FORTRAN SPIN CHARGE MASS WIDTH
%token INPUT DERIVED
%token LAGRANGIAN
%token STAR

%left PLUS MINUS
%nonassoc NEG UPLUS
%left TIMES DIV

%start model
%type < Vertex_syntax.Model.t > model

%%

model:
 | declarations END { $1 }
;

declarations:
 |                                 { M.empty }
 | declarations particle           { $1 }
 | declarations parameter          { $1 }
 | declarations set_lagrangian     { M.l $2 M.empty }
 | declarations augment_lagrangian { M.l $2 $1 }
;

particle:
 | NEUTRAL token_list_arg particle_attributes      { M.empty }
 | CHARGED token_list_arg_pair particle_attributes { M.empty }
;

fortran_token_list_arg:
 | LBRACE fortran_token_list RBRACE { $2 }
;

token_list_arg:
 | LBRACE token_list RBRACE { $2 }
;

token_list_arg_pair:
 | token_list_arg token_list_arg { ($1, $2) }
;

particle_attributes:
 | particle_attribute                     { [ $1 ] }
 | particle_attribute particle_attributes { $1 :: $2 }
;

particle_attribute:
 |      ALIAS   token_list_arg           { () }
 | ANTI ALIAS   token_list_arg           { () }
 |      TEX     token_list_arg           { () }
 | ANTI TEX     token_list_arg           { () }
 |      FORTRAN fortran_token_list_arg   { () }
 | ANTI FORTRAN fortran_token_list_arg   { () }
 |      SPIN    arg                      { () }
 |      CHARGE  arg                      { () }
 |      MASS    fortran_token_list_arg   { () }
 |      WIDTH   fortran_token_list_arg   { () }
;

parameter:
 | INPUT   fortran_token_list_arg arg parameter_attributes { M.empty }
 | DERIVED fortran_token_list_arg arg parameter_attributes { M.empty }
;

parameter_attributes:
 | parameter_attribute                      { [ $1 ] }
 | parameter_attribute parameter_attributes { $1 :: $2 }
;

parameter_attribute:
 |      ALIAS   token_list_arg            { () }
 |      TEX     token_list_arg            { () }
 |      FORTRAN token_list_arg            { () }
;

set_lagrangian: /* We'd like to avoid the STAR in the first clause. */
 | LAGRANGIAN EQUAL STAR vertex                { (E.integer 1, $4) }
 | LAGRANGIAN EQUAL expr STAR vertex           { ($3, $5) }
 | LAGRANGIAN EQUAL expr                       { ($3, V.List []) }
;

augment_lagrangian: /* We'd like to avoid the STAR in the first clause. */
 | LAGRANGIAN PLUS EQUAL STAR vertex           { (E.integer 1, $5) }
 | LAGRANGIAN PLUS EQUAL expr STAR vertex      { ($4, $6) }
 | LAGRANGIAN PLUS EQUAL expr                  { ($4, V.List []) }
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
 | token_list      { V.List $1 }
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
 | DIGIT  { V.Digit $1 }
 | NAME   { V.Name $1 }
 | PLUS   { V.Name "+" }
 | MINUS  { V.Name "-" }
 | TIMES  { V.Name "*" }
 | DIV    { V.Name "/" }
 | COMMA  { V.Name "," }
 | LPAREN { V.Name "(" }
 | RPAREN { V.Name ")" }
;

fortran_token_list:
 | fortran_token                    { [$1] }
 | fortran_token fortran_token_list { $1 :: $2 }
   /* Right recursion is more convenient for constructing
      the value.  Since the lists will always be short,
      there is no performace or stack size reason for
      prefering left recursion. */
;

fortran_token:
 | DIGIT  { V.Digit $1 }
 | NAME   { V.Name $1 }
 | SUB    { V.Name "_" }
;
