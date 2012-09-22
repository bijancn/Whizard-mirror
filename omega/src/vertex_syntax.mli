(* $Id: vertex_syntax.mli 3670 2012-01-21 19:33:07Z jr_reuter $

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(* \thocwmodulesection{Abstract Syntax} *)

type scalar
type vector and vatom
type tensor and tatom
type spinor and satom
type conj_spinor and catom
type vector_spinor and vsatom
type vector_conj_spinor and vcatom

type scalar_current = S | P | SL | SR
type vector_current = V | A | VL | VR
type tensor_current = T

(* [index] denotes the ordinal number of field in the vertex (counting from~$1$).
   E.\,g.
   \begin{verbatim}
vertex e+, e-, A : { e * <1|V|2>.e3 }
vertex nuebar, W+, e- : { g * <1|(V-A)|3>.e2 }
   \end{verbatim}
   denote~$e\cdot\bar{\mathrm{e}}\fmslash{\mathrm{A}}\mathrm{e}$
   and~$g\cdot\bar\nu_{\mathrm{e}}\fmslash{\mathrm{W}}^+(1-\gamma_5)\mathrm{e}$,
   respectively. *)
type index = int

(* Scalar constructors: *)

val null : unit -> scalar
val i : unit -> scalar
val integer : int -> scalar
val constant : string -> scalar
val fraction : scalar -> int -> scalar
val multiple : int -> scalar -> scalar

val scalar_current : scalar_current -> index -> index -> scalar

val mul : scalar -> scalar -> scalar
val add : scalar -> scalar -> scalar
val sub : scalar -> scalar -> scalar

val dot : vatom -> vatom -> scalar
val eps : vatom -> vatom -> vatom -> vatom -> scalar

(* Vector constructors: *)

val e : index -> vatom
val k : index -> vatom
val x : string -> vatom

val vector_current : vector_current -> index -> index -> vatom

val addv : vatom -> vatom -> vatom
val subv : vatom -> vatom -> vatom

val pseudo : vatom -> vatom -> vatom -> vatom

val contract_left : vatom -> tatom -> vatom
val contract_right : tatom -> vatom -> vatom

(* Spinor constructors: *)

val vatom_vsatom : vatom -> vsatom -> spinor
val vatom_vcatom : vatom -> vcatom -> conj_spinor
 
(* Tensor constructors: *)

val tensor_current : tensor_current -> index -> index -> tatom

(* Partial derivatives: *)

val partial_vector : vatom -> scalar -> vector
val partial_spinor : index -> scalar -> conj_spinor
val partial_conj_spinor : index -> scalar -> spinor

(* \thocwmodulesection{Diagnostics} *)

val scalar_to_string : scalar -> string
val vector_to_string : vector -> string
val spinor_to_string : spinor -> string
val conj_spinor_to_string : conj_spinor -> string

type atoms =
    private { constants : string list;
              momenta : index list;
              polarizations : index list;
              external_momenta : string list;
              spinors : index list;
              conj_spinors : index list }

val scalar_atoms : scalar -> atoms

exception Syntax_Error of string * int * int

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  compile-command:"ocamlc -o vertex thoList.ml{i,} pmap.ml{i,} vertex.ml"
 *  End:
i*)
