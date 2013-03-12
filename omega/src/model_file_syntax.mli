(* $Id: model_file_syntax.mli 4105 2013-03-12 16:53:22Z ohl $

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(* This is not supposed to be an abstract data type, just the skeleton that
   the parser is based on. *)

type name =
  | Charged of string * string
  | Neutral of string

type particle = { name : name; attribs : (string * string) list }
val charged : string -> string -> (string * string) list -> particle
val neutral : string -> (string * string) list -> particle

type vertex = { fields : string list; expr : Vertex_syntax.scalar }
val vertex : string list -> string -> vertex

type coupling = string
val coupling : string -> coupling

type file =
    { particles : particle list;
      couplings : coupling list;
      vertices : vertex list;
      authors : string list;
      version : string list;
      created : string list;
      revised : string list }

val empty : unit -> file
val add_particle : particle -> file -> file
val add_coupling : string -> file -> file
val add_vertex : vertex -> file -> file
val add_author : string -> file -> file
val add_version : string -> file -> file
val add_created : string -> file -> file
val add_revised : string -> file -> file

exception Syntax_Error of string * int * int

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

