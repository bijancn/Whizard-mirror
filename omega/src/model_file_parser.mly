/* $Id: model_file_parser.mly 4105 2013-03-12 16:53:22Z ohl $

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
  raise (Model_file_syntax.Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < string > STRING EXPR
%token PARTICLE COUPLING VERTEX
%token AUTHOR VERSION CREATED REVISED
%token COMMA EQUAL COLON
%token END

%start file
%type < Model_file_syntax.file > file

%%

file:
    declarations END           { $1 }
;

declarations:
                               { Model_file_syntax.empty () }
  | declarations particle_declaration
                               { Model_file_syntax.add_particle $2 $1 }
  | declarations vertex_declaration
                               { Model_file_syntax.add_vertex $2 $1 }
  | declarations coupling_declaration
                               { Model_file_syntax.add_coupling $2 $1 }
  | declarations AUTHOR EXPR   { Model_file_syntax.add_author $3 $1 }
  | declarations VERSION EXPR  { Model_file_syntax.add_version $3 $1 }
  | declarations CREATED EXPR  { Model_file_syntax.add_created $3 $1 }
  | declarations REVISED EXPR  { Model_file_syntax.add_revised $3 $1 }
;

particle_declaration:
    PARTICLE STRING attrib_list
                               { Model_file_syntax.neutral $2 $3 }
  | PARTICLE STRING opt_comma STRING attrib_list
                               { Model_file_syntax.charged $2 $4 $5 }
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
    COUPLING STRING            { Model_file_syntax.coupling $2 }
;

vertex_declaration:
    VERTEX particle_list COLON EXPR
                               { Model_file_syntax.vertex $2 $4 }
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
