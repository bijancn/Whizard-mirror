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

/* Right recursion is more convenient for constructing
   the value.  Since the lists will always be short,
   there is no performace or stack size reason for
   prefering left recursion. */

%{
module T = Vertex_syntax.Token
module E = Vertex_syntax.Expr
module P = Vertex_syntax.Particle
module V = Vertex_syntax.Parameter
module F = Vertex_syntax.File_Tree
module M = Vertex_syntax.Model
let parse_error msg =
  raise (Vertex_syntax.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < int > DIGIT
%token < string > TOKEN
%token SUPER SUB LBRACE RBRACE LBRACKET RBRACKET
%token LPAREN RPAREN
%token COMMA
%token PLUS MINUS TIMES DIV EQUAL

%token < string > INCLUDE
%token END

%token NEUTRAL CHARGED
%token ANTI ALIAS TEX FORTRAN SPIN CHARGE MASS WIDTH
%token INPUT DERIVED
%token LAGRANGIAN
%token STAR

%left PLUS MINUS
%nonassoc NEG UPLUS
%left TIMES DIV

%start file
%type < Vertex_syntax.File_Tree.t > file

%%

file:
 | declarations END { $1 }
;

declarations:
 |                                 { [] }
 | declaration declarations        { $1 :: $2 }
;

declaration:
 | particle           { F.Particle $1 }
 | parameter          { F.Parameter $1 }
 | lagrangian         { let e, t = $1 in
			F.Lagrangian (e, t) }
 | INCLUDE            { F.Include $1 }
;

particle:
 | NEUTRAL token_list_arg particle_attributes
     { { P.name = P.Neutral $2; P.attr = $3 } }
 | CHARGED token_list_arg_pair particle_attributes
     { let p, ap = $2 in
       { P.name = P.Charged (p, ap); P.attr = $3 } }
;

fortran_token_list_arg:
 | LBRACE fortran_token_list RBRACE { $2 }
;

expr_arg:
 | LBRACKET expr RBRACKET             { $2 }
;

token_list_arg:
 | LBRACE token_list RBRACE { $2 }
;

token_list_arg_pair:
 | token_list_arg token_list_arg { ($1, $2) }
;

particle_attributes:
 |                                        { [ ] }
 | particle_attribute particle_attributes { $1 :: $2 }
;

particle_attribute:
 |      ALIAS   token_list_arg           { P.Alias $2 }
 | ANTI ALIAS   token_list_arg           { P.Alias_Anti $3 }
 |      TEX     token_list_arg           { P.TeX $2 }
 | ANTI TEX     token_list_arg           { P.TeX_Anti $3 }
 |      FORTRAN fortran_token_list_arg   { P.Fortran $2 }
 | ANTI FORTRAN fortran_token_list_arg   { P.Fortran_Anti $3 }
 |      SPIN    arg                      { P.Spin $2 }
 |      CHARGE  arg                      { P.Charge $2 }
 |      MASS    fortran_token_list_arg   { P.Mass $2 }
 |      WIDTH   fortran_token_list_arg   { P.Width $2 }
;

parameter:
 | INPUT   fortran_token_list_arg arg parameter_attributes
     { V.Input { V.name = $2; V.value = $3; V.attr = $4 } }
 | DERIVED fortran_token_list_arg arg parameter_attributes
     { V.Derived { V.name = $2; V.value = $3; V.attr = $4 } }
;

parameter_attributes:
 |                                          { [ ] }
 | parameter_attribute parameter_attributes { $1 :: $2 }
;

parameter_attribute:
 | ALIAS   token_list_arg { V.Alias $2 }
 | TEX     token_list_arg { V.TeX $2 }
 | FORTRAN token_list_arg { V.Fortran $2 }
;

lagrangian:
 | LAGRANGIAN token_list_arg          { (E.integer 1, T.List $2) }
 | LAGRANGIAN expr_arg token_list_arg { ($2, T.List $3) }
 | LAGRANGIAN expr_arg LBRACE RBRACE  { ($2, T.List []) }
;

expr:
 | integer                 	{ E.integer $1 }
 | LPAREN expr RPAREN      	{ $2 }
 | expr PLUS expr          	{ E.add $1 $3 }
 | expr MINUS expr         	{ E.sub $1 $3 }
 | expr TIMES expr         	{ E.mult $1 $3 }
 | expr DIV expr           	{ E.div $1 $3 }
 | bare_scripted_token arg_list { E.apply $1 $2 }
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

token_list:
 | scripted_token            { [$1] }
 | scripted_token token_list { $1 :: $2 }
;

scripted_token:
 | token
     { T.Scripted { T.token = $1; T.super = []; T.sub = [] } }
 | token SUPER token
     { T.Scripted { T.token = $1; T.super = T.plug $3; T.sub = [] } }
 | token SUB token
     { T.Scripted { T.token = $1; T.super = []; T.sub = T.plug $3 } }
 | token SUPER token SUB token
     { T.Scripted { T.token = $1; T.super = T.plug $3; T.sub = T.plug $5 } }
 | token SUB token SUPER token
     { T.Scripted { T.token = $1; T.super = T.plug $5; T.sub = T.plug $3 } }
;

bare_scripted_token:
 | TOKEN
     { T.Scripted { T.token = T.Token $1; T.super = []; T.sub = [] } }
 | TOKEN SUPER token
     { T.Scripted { T.token = T.Token $1; T.super = T.plug $3; T.sub = [] } }
 | TOKEN SUB token
     { T.Scripted { T.token = T.Token $1; T.super = []; T.sub = T.plug $3 } }
 | TOKEN SUPER token SUB token
     { T.Scripted { T.token = T.Token $1; T.super = T.plug $3; T.sub = T.plug $5 } }
 | TOKEN SUB token SUPER token
     { T.Scripted { T.token = T.Token $1; T.super = T.plug $5; T.sub = T.plug $3 } }
;

token:
 | bare_token
     { $1 }
 | LBRACE token RBRACE
     { $2 }
 | LBRACE token token_list RBRACE
     { T.List ($2 :: $3) }
;

bare_token:
 | DIGIT    { T.Digit $1 }
 | TOKEN    { T.Token $1 }
 | PLUS     { T.Token "+" }
 | MINUS    { T.Token "-" }
 | TIMES    { T.Token "*" }
 | DIV      { T.Token "/" }
 | COMMA    { T.Token "," }
 | LPAREN   { T.Token "(" }
 | RPAREN   { T.Token ")" }
 | LBRACKET { T.Token "[" }
 | RBRACKET { T.Token "]" }
;

fortran_token_list:
 | fortran_token                    { [$1] }
 | fortran_token fortran_token_list { $1 :: $2 }
;

fortran_token:
 | DIGIT  { T.Digit $1 }
 | TOKEN  { T.Token $1 }  /* This also accepts TeX command sequences ... */
 | SUB    { T.Token "_" }
;
