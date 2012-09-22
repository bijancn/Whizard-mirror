(* $Id: vertex.ml 3670 2012-01-21 19:33:07Z jr_reuter $

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

open Vertex_syntax

let parse text =
  try
    Vertex_parser.coupling Vertex_lexer.token (Lexing.from_string text)
  with
  | Vertex_syntax.Syntax_Error (msg, i, j) ->
      invalid_arg (Printf.sprintf "syntax error (%s) at: `%s'"
                     msg  (String.sub text i (j - i + 1)))
  | Parsing.Parse_error -> invalid_arg ("parse error: " ^ text)

(*i
let tgv = parse
    "(k1.e3 - k2.e3)*e1.e2 + (k2.e1 - k3.e1)*e2.e3 + (k3.e2 - k1.e2)*e3.e1"

let tgv = parse
    "(k1 - k2).e3*e1.e2 + (k2 - k3).e1*e2.e3 + (k3 - k1).e2*e3.e1"
i*)

type wf =
    { lorentz : Coupling.lorentz;
      momentum : bool }

type vertex =
    { coupling : Vertex_syntax.scalar;
      wfs : wf list }

let take_nth n list =
  let rec take_nth' i rev_head tail =
    if i < 0 then
      invalid_arg "take_nth"
    else if i = 0 then
      match tail with
      | [] -> invalid_arg "take_nth"
      | x :: tail' -> (x, List.rev_append rev_head tail')
    else
      match tail with
      | [] -> invalid_arg "take_nth"
      | x :: tail' -> take_nth' (pred i) (x :: rev_head) tail'
  in
  take_nth' n [] list

module Fortran =
  struct
    let type_of_lorentz kind = function
      | Coupling.Scalar -> "complex(kind=" ^ kind ^ ")"
      | Coupling.Spinor -> "type(spinor)"
      | Coupling.ConjSpinor -> "type(conjspinor)"
      | Coupling.Majorana -> "type(bispinor)"
      | Coupling.Maj_Ghost -> assert false
      | Coupling.Vector | Coupling.Massive_Vector -> "type(vector)"
      | Coupling.Vectorspinor -> assert false
      | Coupling.Tensor_1 -> assert false
      | Coupling.Tensor_2 -> assert false
      | Coupling.BRS _ -> assert false

    let mnemonic = function
      | Coupling.Scalar -> "phi"
      | Coupling.Spinor -> "psi"
      | Coupling.ConjSpinor -> "psibar"
      | Coupling.Majorana -> "chi"
      | Coupling.Maj_Ghost -> assert false
      | Coupling.Vector | Coupling.Massive_Vector -> "V"
      | Coupling.Vectorspinor -> assert false
      | Coupling.Tensor_1 -> assert false
      | Coupling.Tensor_2 -> assert false
      | Coupling.BRS _ -> assert false

    let declare_wf ?(kind = "default") i wf =
      Printf.printf "  %s, intent(in) :: %s%d\n"
        (type_of_lorentz kind wf.lorentz) (mnemonic wf.lorentz) (succ i);
      if wf.momentum then begin
        Printf.printf "  type(momentum), intent(in) :: k%d\n" (succ i);
        Printf.printf "  type(vector) :: k%dv\n" (succ i)
      end

    let vector_of_momentum i wf =
      if wf.momentum then begin
        Printf.printf "  k%dv = k%d\n" (succ i) (succ i)
      end

    let print_fusion name i v =
      let result, children = take_nth i v.wfs in
      let result_name = mnemonic result.lorentz
      and result_type = type_of_lorentz "default" result.lorentz in
      let children = Array.of_list children in
      Printf.printf "pure function %s (%s) result (%s)\n"
        name "???" result_name;
      Array.iteri declare_wf children;
      Printf.printf "  %s :: %s\n" result_type result_name;
      if result.momentum then
        begin
          Printf.printf "  type(momentum), intent(in) :: k\n";
          Printf.printf "  k = \n"
        end;
      Array.iteri vector_of_momentum children;
      Printf.printf "end function %s\n" name

  end

(* NB:
   \begin{dubious}
      If the outgoing momentum is used, \emph{all} the incoming momenta
      must be passed too, unless the outgoing momentum is passed itself.
   \end{dubious} *)

(*i module IMap = Map.Make (struct type t = int let compare = compare end) i*)

let insert_scalars order wfs = 
  let rec insert_scalars' n order = function
    | [] -> []
  in
  insert_scalars' 0 order wfs
	  

let wfs order atoms =
  List.sort (fun (n1, _) (n2, _) -> compare n1 n2)
    (List.map (fun n -> (n, { lorentz = Coupling.Vector;
			      momentum = List.mem n atoms.momenta })) atoms.polarizations @
     List.map (fun n -> (n, { lorentz = Coupling.Spinor;
			      momentum = List.mem n atoms.momenta })) atoms.spinors @
     List.map (fun n -> (n, { lorentz = Coupling.ConjSpinor;
			   momentum = List.mem n atoms.momenta })) atoms.conj_spinors)

open Fortran
open Printf

let process_vertex coupling =
  let order = 3 in
  printf ">>>>>>>> %s\n" (scalar_to_string coupling);
  let atoms = scalar_atoms coupling in
  printf "         constants: %s\n"
    (String.concat ", " atoms.constants);
  printf "           momenta: %s\n"
    (String.concat ", " (List.map string_of_int atoms.momenta));
  printf "     polarizations: %s\n"
    (String.concat ", " (List.map string_of_int atoms.polarizations));
  printf "  external momenta: %s\n"
    (String.concat ", " atoms.external_momenta);
  printf "           spinors: %s\n"
    (String.concat ", " (List.map string_of_int atoms.spinors));
  printf "conjugated spinors: %s\n"
    (String.concat ", " (List.map string_of_int atoms.conj_spinors));
  printf "d/deps1: %s\n" (vector_to_string (partial_vector (e 1) coupling));
  printf "d/deps2: %s\n" (vector_to_string (partial_vector (e 2) coupling));
  printf "d/deps3: %s\n" (vector_to_string (partial_vector (e 3) coupling));
  printf "d/|1>: %s\n" (conj_spinor_to_string (partial_spinor 1 coupling));
  printf "d/|2>: %s\n" (conj_spinor_to_string (partial_spinor 2 coupling));
  printf "d/|3>: %s\n" (conj_spinor_to_string (partial_spinor 3 coupling));
  printf "d/<1|: %s\n" (spinor_to_string (partial_conj_spinor 1 coupling));
  printf "d/<2|: %s\n" (spinor_to_string (partial_conj_spinor 2 coupling));
  printf "d/<3|: %s\n" (spinor_to_string (partial_conj_spinor 3 coupling));
  print_fusion "foo" 0
    { coupling = coupling;
      wfs = List.map snd (wfs order atoms) };
  print_fusion "foo" 1
    { coupling = coupling;
      wfs = List.map snd (wfs order atoms) };
  print_fusion "foo" 2
    { coupling = coupling;
      wfs = List.map snd (wfs order atoms) }

let process_vertex coupling =
  try
    process_vertex coupling
  with
  | Failure s ->
      printf "************************************************************************\n";
      printf "FAILURE: %s!!!\n" s;
      printf "************************************************************************\n"

(*i
let _ =
  process_vertex (parse (read_line ()))
i*)

(* \thocwmodulesection{Code Generation}
   \begin{dubious}
     Most of this will be moved to [Targets].
   \end{dubious} *)

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  compile-command:"ocamlc -o vertex thoList.ml{i,} pmap.ml{i,} vertex.ml"
 *  End:
i*)
