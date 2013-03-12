(* $Id: vertex.mli 4105 2013-03-12 16:53:22Z ohl $

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

module Test : functor (M : Model.T) ->
  sig val example : unit -> unit val suite : OUnit.test end

(*i 

(* We're dealing with the tensor algebra freely generated by
   momenta, metric and $\epsilon$ tensors, as well as scalars,
   vectors and tensors constructed from fermionic bilinears.

   The design problem that we're dealing with is that an implementation
   relying on types to guarantee that only legal expressions can be
   constructed will be hideously complex.  A ``correct'' solution would
   represent vertices as tensors, without using indices, external
   polarization vectors or currents.  However, the presence of
   contractions~$g^{\mu\nu}$ and~$\epsilon^{\mu\nu\rho\sigma}$ introduces
   a wealth of special cases, corresponding to which combinations of invariant
   tensors remains uncontracted.

   Therefore, it appears to be a better strategy to use arithmetic expressions
   built from tensors contrated with external polarization vectors.  We can then
   check at runtime that the expression is linear in these polarization vectors. *)

(* \thocwmodulesection{Code Generation}
   \begin{dubious}
     Most of this will be moved to [Targets].
   \end{dubious} *)

val parse : string -> Vertex_syntax.scalar

val process_vertex : Vertex_syntax.scalar -> unit

i*)

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  compile-command:"ocamlc -o vertex thoList.ml{i,} pmap.ml{i,} vertex.ml"
 *  End:
i*)
