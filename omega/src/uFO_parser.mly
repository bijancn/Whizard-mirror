/* $Id: vertex_parser.mly 7444 2016-02-17 15:37:20Z jr_reuter $

   Copyright (C) 1999-2016 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
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
module U = UFO_syntax

let parse_error msg =
  raise (UFO_syntax.Syntax_Error
	   (msg, symbol_start_pos (), symbol_end_pos ()))

let invalid_parameter_attr () =
  parse_error "invalid parameter attribute"

%}

%token < int > INT
%token < float > FLOAT
%token < string > STRING ID
%token DOT COMMA COLON
%token EQUAL PLUS MINUS DIV
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET

%token END

%start file
%type < UFO_syntax.t > file

%%

file:
 | declarations END { $1 }
;

declarations:
 |                          { [] }
 | declaration declarations { $1 :: $2 }
;

declaration:
 | ID EQUAL name LPAREN RPAREN            { { U.name = $1;
					      U.kind = $3;
					      U.attribs = [] } }
 | ID EQUAL name LPAREN attributes RPAREN { { U.name = $1;
					      U.kind = $3;
					      U.attribs = $5 } }
 | ID EQUAL STRING                        { { U.name = $1;
					      U.kind = ["$"; $3]; (* HACK! *)
					      U.attribs = [] } }
;

name:
 | ID          { [$1] }
 | name DOT ID { $3 :: $1 }
;

attributes:
 | attribute                  { [$1] }
 | attribute COMMA attributes { $1 :: $3 }
;

attribute:
 | ID EQUAL INT                        { { U.a_name = $1;
					   U.a_value = U.Integer $3 } }
 | ID EQUAL INT DIV INT                { { U.a_name = $1;
					   U.a_value = U.Fraction ($3, $5) } }
 | ID EQUAL FLOAT                      { { U.a_name = $1;
					   U.a_value = U.Float $3 } }
 | ID EQUAL STRING                     { { U.a_name = $1;
					   U.a_value = U.String $3 } }
 | ID EQUAL name                       { { U.a_name = $1;
					   U.a_value = U.Name $3 } }
 | ID EQUAL LBRACKET RBRACKET 	       { { U.a_name = $1;
					   U.a_value = U.List [] } }
 | ID EQUAL LBRACKET names RBRACKET    { { U.a_name = $1;
					   U.a_value = U.List $4 } }
 | ID EQUAL LBRACKET strings RBRACKET  { { U.a_name = $1;
					   U.a_value = U.List $4 } }
 | ID EQUAL LBRACKET integers RBRACKET { { U.a_name = $1;
					   U.a_value = U.List $4 } }
 | ID EQUAL LBRACE orders RBRACE       { { U.a_name = $1;
					   U.a_value = U.Dictionary $4 } }
 | ID EQUAL LBRACE couplings RBRACE    { { U.a_name = $1;
					   U.a_value = U.Dictionary $4 } }
;

names:
 | name             { [U.Name $1] }
 | name COMMA names { U.Name $1 :: $3 }
;

integers:
 | INT                { [U.Integer $1] }
 | INT COMMA integers { U.Integer $1 :: $3 }
;

strings:
 | STRING               { [U.String $1] }
 | STRING COMMA strings { U.String $1 :: $3 }
;

orders:
 | order              { [$1] }
 | order COMMA orders { $1 :: $3 }
;

order:
 | STRING COLON INT { U.Order ($1, $3) }
;

couplings:
 | coupling                 { [$1] }
 | coupling COMMA couplings { $1 :: $3 }
;

coupling:
 | LPAREN INT COMMA INT RPAREN COLON name { U.Coupling ($2, $4, $7) }
;

