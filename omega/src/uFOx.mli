(* $Id: vertex.mli 7444 2016-02-17 15:37:20Z jr_reuter $

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

module Expr :
  sig
    type t
    val of_string : string -> t
  end

module Lorentz :
  sig
    type t
    val of_expr : Expr.t -> t
    val of_string : string -> t
    val to_string : t -> string
    type index_classes
    val classify_indices : t -> index_classes
    val index_classes_to_string : index_classes -> string
  end

module Color :
  sig
    type t
    val of_expr : Expr.t -> t
    val of_string : string -> t
    val to_string : t -> string
    type index_classes
    val classify_indices : t -> index_classes
    val index_classes_to_string : index_classes -> string
  end

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end
