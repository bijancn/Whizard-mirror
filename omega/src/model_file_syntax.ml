(* $Id: model_file_syntax.ml -1   $

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

type name =
  | Charged of string * string
  | Neutral of string

type particle = { name : name; attribs : (string * string) list }
type vertex = { fields : string list; expr : Vertex_syntax.scalar }
type coupling = string

type file =
    { particles : particle list;
      couplings : coupling list;
      vertices : vertex list;
      authors : string list;
      version : string list;
      created : string list;
      revised : string list }

let empty () =
  { particles = [];
    couplings = [];
    vertices = [];
    authors = [];
    version = [];
    created = [];
    revised = [] }

let add_particle particle file =
  { file with particles = particle :: file.particles }

let add_coupling coupling file =
  { file with couplings = coupling :: file.couplings }

let add_vertex vertex file =
  { file with vertices = vertex :: file.vertices }

let add_author author file =
  { file with authors = author :: file.authors }

let add_version version file =
  { file with version = version :: file.version }

let add_created created file =
  { file with created = created :: file.created }

let add_revised revised file =
  { file with revised = revised :: file.revised }

let neutral name attribs =
  { name = Neutral name; attribs = attribs }

let charged name anti attribs =
  { name = Charged (name, anti); attribs = attribs }

let coupling name = name

let vertex fields expr =
  { fields = fields; expr = Vertex.parse expr }

exception Syntax_Error of string * int * int

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

