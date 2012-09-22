/* $Id: vertex_parser.mly 3670 2012-01-21 19:33:07Z jr_reuter $

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

%{
let parse_error msg =
  raise (Vertex_syntax.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < int > INT
%token < string > NAME
%token < int > POLARIZATION MOMENTUM
%token EPSILON
%token S P V A T
%token I
%token LPAREN RPAREN BRA VERT KET LEXT REXT COMMA
%token PLUS MINUS TIMES DIV DOT POWER
%token END

%left PLUS MINUS
%nonassoc NEG UPLUS
%left TIMES
%left DIV
%right POWER
%left DOT

%start coupling
%type < Vertex_syntax.scalar > coupling

%%

coupling:
    expr END                 { $1 }
  | END                      { Vertex_syntax.null () }
;

expr:
    contraction              { $1 }
  | I                        { Vertex_syntax.i () }
  | INT                      { Vertex_syntax.integer $1 }
  | NAME                     { Vertex_syntax.constant $1 }
  | expr DIV INT             { Vertex_syntax.fraction $1 $3 }
  | INT TIMES expr           { Vertex_syntax.multiple $1 $3 }
  | LPAREN expr RPAREN       { $2 }
  | expr TIMES expr          { Vertex_syntax.mul $1 $3 }
  | expr PLUS expr           { Vertex_syntax.add $1 $3 }
  | expr MINUS expr          { Vertex_syntax.sub $1 $3 }
  | MINUS expr %prec NEG     { Vertex_syntax.sub (Vertex_syntax.null ()) $2 }
  | PLUS expr %prec UPLUS    { $2 }
  | bra scalar_current ket   { Vertex_syntax.scalar_current $2 $1 $3 }
  | bra vector_current_dot ket
                             { let (c, v) = $2 in
                               Vertex_syntax.dot (Vertex_syntax.vector_current c $1 $3) v }
  | EPSILON LPAREN vector COMMA vector COMMA vector COMMA vector RPAREN
                             { Vertex_syntax.eps $3 $5 $7 $9 }
;

vector_current_dot:
    vector_current DOT vector
                             { ($1, $3) }
  | vector DOT vector_current
                             { ($3, $1) }
  | vector_current DOT vector_current
                             { parse_error "contracted gamma matrices" }
;

contraction:
    vector DOT vector        { Vertex_syntax.dot $1 $3 }
;

vector:
    POLARIZATION             { Vertex_syntax.e $1 }
  | MOMENTUM                 { Vertex_syntax.k $1 }
  | LEXT NAME REXT           { Vertex_syntax.x $2 }
  | LPAREN vector RPAREN     { $2 }
  | vector PLUS vector       { Vertex_syntax.addv $1 $3 }
  | vector MINUS vector      { Vertex_syntax.subv $1 $3 }
  | vector DOT tensor        { Vertex_syntax.contract_left $1 $3 }
  | tensor DOT vector        { Vertex_syntax.contract_right $1 $3 }
  | vector TIMES vector      { parse_error "vector*vector" }
  | vector DIV vector        { parse_error "vector/vector" }
  | bra vector_current ket   { Vertex_syntax.vector_current $2 $1 $3 }
  | EPSILON LPAREN vector COMMA vector COMMA vector RPAREN
                             { Vertex_syntax.pseudo $3 $5 $7 }
;

tensor:
    bra tensor_current ket   { Vertex_syntax.tensor_current $2 $1 $3 }
;

scalar_current:
    S                        { Vertex_syntax.S }
  | P                        { Vertex_syntax.P }
  | S MINUS P                { Vertex_syntax.SL }
  | S PLUS P                 { Vertex_syntax.SR }
  | S plus_minus S           { parse_error "S+/-S" }
  | S plus_minus V           { parse_error "S+/-V" }
  | S plus_minus A           { parse_error "S+/-A" }
  | LPAREN scalar_current RPAREN
                             { $2 }
;

vector_current:
    V                        { Vertex_syntax.V }
  | A                        { Vertex_syntax.A }
  | V MINUS A                { Vertex_syntax.VL }
  | V PLUS A                 { Vertex_syntax.VR }
  | LPAREN vector_current RPAREN
                             { $2 }
;

tensor_current:
    T                        { Vertex_syntax.T }
  | LPAREN tensor_current RPAREN
                             { $2 }
;

plus_minus:
    PLUS                     { }
  | MINUS                    { }
;
bra:
    BRA INT VERT             { $2 }
;

ket:
    VERT INT KET             { $2 }
;
