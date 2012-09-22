/* $Id: model_parser.mly 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
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
  raise (Model_syntax.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < string > STRING EXPR
%token PARTICLE COUPLING VERTEX
%token AUTHOR VERSION CREATED REVISED
%token COMMA EQUAL COLON
%token END

%start file
%type < Model_syntax.file > file

%%

file:
    declarations END           { $1 }
;

declarations:
                               { Model_syntax.empty () }
  | declarations particle_declaration
                               { Model_syntax.add_particle $2 $1 }
  | declarations vertex_declaration
                               { Model_syntax.add_vertex $2 $1 }
  | declarations coupling_declaration
                               { Model_syntax.add_coupling $2 $1 }
  | declarations AUTHOR EXPR   { Model_syntax.add_author $3 $1 }
  | declarations VERSION EXPR  { Model_syntax.add_version $3 $1 }
  | declarations CREATED EXPR  { Model_syntax.add_created $3 $1 }
  | declarations REVISED EXPR  { Model_syntax.add_revised $3 $1 }
;

particle_declaration:
    PARTICLE STRING attrib_list
                               { Model_syntax.neutral $2 $3 }
  | PARTICLE STRING opt_comma STRING attrib_list
                               { Model_syntax.charged $2 $4 $5 }
;

attrib_list:
                               { List.rev [] }
  | COLON                      { List.rev [] }
  | COLON rev_attrib_list      { List.rev $2 }

rev_attrib_list:
    attrib                     { [$1] }
  | rev_attrib_list opt_comma attrib
                               { $3 :: $1 }
;

attrib:
    STRING                     { ($1, "true") }
  | STRING EQUAL STRING        { ($1, $3) }
;

coupling_declaration:
    COUPLING STRING            { Model_syntax.coupling $2 }
;

vertex_declaration:
    VERTEX particle_list COLON EXPR
                               { Model_syntax.vertex $2 $4 }
;

particle_list:
    rev_particle_list          { List.rev $1 }

rev_particle_list:
    STRING                     { [$1] }
  | rev_particle_list opt_comma STRING
                               { $3 :: $1 }
;

opt_comma:
                               { () }
  | COMMA                      { () }
;
