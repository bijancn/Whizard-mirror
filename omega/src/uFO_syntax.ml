(* $Id: vertex_syntax.ml 7444 2016-02-17 15:37:20Z jr_reuter $

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(* \thocwmodulesection{Abstract Syntax} *)

exception Syntax_Error of string * Lexing.position * Lexing.position

type name = string list

type entry =
  | Order of string * int
  | Coupling of int * int * name

type dictionary = entry list
  
type value =
  | Name of name
  | Integer of int
  | Float of float
  | Fraction of int * int
  | String of string
  | Dictionary of dictionary
  | List of value list

type attrib =
  { a_name : string;
    a_value : value }
  
type declaration =
  { name : string;
    kind : name;
    attribs : attrib list }

type t = declaration list

let to_strings declarations =
  []
