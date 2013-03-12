(* $Id: vertex.ml 4105 2013-03-12 16:53:22Z ohl $

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

type context =
    { arity : int;
      lorentz_reps : Coupling.lorentz array;
      color_reps : Color.t array }

(* An abstract type that allows us to distinguish offsets
   in the field array, from color and Lorentz indices in
   different representations. *)

module type Index =
  sig
    type t
    val of_int : int -> t
    val to_int : t -> int
  end

module Field : Index =
  struct
    type t = int
    let of_int i = i
    let to_int i = i
  end

type field = Field.t

module type Lorentz =
  sig
  end

module Lorentz (* : Lorentz *) =
  struct

    module Index : Index =
      struct
        type t = int
        let of_int i = i
        let to_int i = i
      end

    type index = Index.t

    type primitive =
      | G of index * index
      | E of index * index * index * index
      | K of index * field
      | S of field * field
      | V of index * field * field
      | T of index * index * field * field
      | A of index * field * field
      | P of field * field

    let map_primitive fi ff = function
      | G (mu, nu) -> G (fi mu, fi nu)
      | E (mu, nu, rho, sigma) -> E (fi mu, fi nu, fi rho, fi sigma)
      | K (mu, i) -> K (fi mu, ff i)
      | S (i, j) -> S (ff i, ff j)
      | V (mu, i, j) -> V (fi mu, ff i, ff j)
      | T (mu, nu, i, j) -> T (fi mu, fi nu, ff i, ff j)
      | A (mu, i, j) -> A (fi mu, ff i, ff j)
      | P (i, j) -> P (ff i, ff j)

    let primitive_ok context =
      let field_in_bounds i =
        let i' = Field.to_int i in
        0 <= i' && i' < context.arity in
      function
	| G (_, _) | E (_, _, _, _) -> true
	| K (_, i) -> field_in_bounds i
	| S (i, j) | V (_, i, j)
	| T (_, _, i, j)
	| A (_, i, j) | P (i, j) ->
	    i <> j && field_in_bounds i && field_in_bounds j &&
	    (match context.lorentz_reps.(Field.to_int i),
              context.lorentz_reps.(Field.to_int j) with
	    | Coupling.ConjSpinor, Coupling.Spinor -> true
	    | (Coupling.Vectorspinor|Coupling.Majorana), _ ->
		failwith "Lorentz.primitive_ok: incomplete"
	    | _, (Coupling.Vectorspinor|Coupling.Majorana) ->
		failwith "Lorentz.primitive_ok: incomplete"
	    | _, _ -> false)

    let primitive_indices = function
      | G (mu, nu) | T (mu, nu, _, _) -> [mu; nu]
      | E (mu, nu, rho, sigma) -> [mu; nu; rho; sigma]
      | K (mu, _) | V (mu, _, _) | A (mu, _, _) -> [mu]
      | S (_, _) | P (_, _) -> []

    let indices p =
      ThoList.flatmap primitive_indices p

    let contraction_ok p =
      List.for_all
	(fun (n, _) -> n = 2)
	(ThoList.classify (indices p))

    type factor =
      | Integer of int
      | Contraction of primitive list

    let map_factor fi ff = function
      | Integer _ as i -> i
      | Contraction p ->
	  Contraction (List.map (map_primitive fi ff) p)

    let factor_ok context = function
      | Integer _ -> true
      | Contraction p ->
	  List.for_all (primitive_ok context) p && contraction_ok p

    type tensor = factor * primitive list

    let map_tensor fi ff (factor, primitives) =
      (map_factor fi ff factor,
       List.map (map_primitive fi ff) primitives)

    let tensor_ok context (factor, primitives) =
      factor_ok context factor &&
      List.for_all (primitive_ok context) primitives

  end

module type Color =
  sig
  end

module Color (* : Color *) = 
  struct

    module Index : Index =
      struct
        type t = int
        let of_int i = i
        let to_int i = i
      end

    type index = Index.t

    type color =
      | Fundamental of field
      | Conjugate of field
      | Adjoint of field

    type primitive =
      | D of field * field
      | E of field * field * field  (* $SU(3)$ *)
      | T of field * field * field
      | F of field * field * field

    let map_primitive f = function
      | D (i, j) -> D (f i, f j)
      | E (i, j, k) -> E (f i, f j, f k)
      | T (a, i, j) -> T (f a, f i, f j)
      | F (a, b, c) -> F (f a, f b, f c)
      
    let primitive_ok context =
      let field_in_bounds i =
        let i' = Field.to_int i in
        0 <= i' && i' < context.arity in
      function
	| D (i, j) ->
	    i <> j &&  field_in_bounds i && field_in_bounds j &&
	    (match context.color_reps.(Field.to_int i),
              context.color_reps.(Field.to_int j) with
	    | Color.SUN (n1), Color.SUN (n2) ->
		n1 = - n2 && n2 > 0
	    | _, _ -> false)
	| E (i, j, k) ->
	    i <> j &&  i <> k &&  k <> k &&
	    field_in_bounds i && field_in_bounds j &&field_in_bounds k &&
	    (match context.color_reps.(Field.to_int i),
	      context.color_reps.(Field.to_int j),
              context.color_reps.(Field.to_int k) with
	    | Color.SUN (n1), Color.SUN (n2), Color.SUN (n3) ->
		n1 = 3 && n2 = 3 && n3 = 3 ||
		n1 = -3 && n2 = -3 && n3 = -3
	      | _, _, _ -> false)
	| T (a, i, j) ->
	    i <> j && a <> i && a <> j &&
	    field_in_bounds a && field_in_bounds i && field_in_bounds j &&
	    (match context.color_reps.(Field.to_int a),
	      context.color_reps.(Field.to_int i),
              context.color_reps.(Field.to_int j) with
	    | Color.AdjSUN(n1), Color.SUN (n2), Color.SUN (n3) ->
		n1 = n3 && n2 = - n3 && n3 > 0
	    | _, _, _ -> false)
	| F (a, b, c) ->
	    a <> b && a <> b && b <> c &&
	    field_in_bounds a && field_in_bounds b && field_in_bounds c &&
	    (match context.color_reps.(Field.to_int a),
	      context.color_reps.(Field.to_int b),
              context.color_reps.(Field.to_int c) with
	    | Color.AdjSUN(n1), Color.AdjSUN (n2), Color.AdjSUN (n3) ->
		n1 = n2 && n2 = n3 && n1 > 0
	    | _, _, _ -> false)

    let primitive_indices = function
      | D (_, _) -> []
      | E (_, _, _) -> []
      | T (a, _, _) -> [a]
      | F (a, b, c) -> [a; b; c]

    let indices p =
      ThoList.flatmap primitive_indices p

    let contraction_ok p =
      let c = ThoList.classify (indices p) in
      let res = List.for_all
	  (fun (n, _) -> n = 2)
	  (c)
      in
      print_endline
	(String.concat ", "
	   (List.map
	      (fun (n, i) ->
                string_of_int n ^ " * " ^
                string_of_int (Field.to_int i))
	      c));
      flush stdout;
      res

    type factor =
      | Integer of int
      | Contraction of primitive list

    let map_factor f = function
      | Integer _ as i -> i
      | Contraction p ->
	  Contraction (List.map (map_primitive f) p)

    let factor_ok context = function
      | Integer _ -> true
      | Contraction p ->
	  List.for_all (primitive_ok context) p &&
	  contraction_ok p

    type tensor = factor * primitive list

    let map_tensor f (factor, primitives) =
      (map_factor f factor, List.map (map_primitive f) primitives)

    let tensor_ok context (factor, primitives) =
      factor_ok context factor &&
      List.for_all (primitive_ok context) primitives

  end

type t =
    { fields : string array;
      lorentz : Lorentz.tensor list;
      color : Color.tensor list }

module Test (M : Model.T) :
    sig val example : unit -> unit val suite : OUnit.test end =
  struct

    module Permutation = Permutation.Default

    let ok v =
      let fields = Array.map M.flavor_of_string v.fields in
      let context =
	{ arity = Array.length v.fields;
	  lorentz_reps = Array.map M.lorentz fields;
	  color_reps = Array.map M.color fields } in
      List.for_all (Lorentz.tensor_ok context) v.lorentz &&
	List.for_all (Color.tensor_ok context) v.color

    module PM =
      Partial.Make (struct type t = Field.t let compare = compare end)

    let id x = x

    let permute v p =
      let sorted =
        List.map Field.of_int (ThoList.range 0 (Array.length v.fields - 1)) in
      let permute_fields =
        PM.apply (PM.of_lists sorted (List.map Field.of_int p)) in
      { fields = Permutation.array (Permutation.of_list p) v.fields;
 	lorentz = List.map (Lorentz.map_tensor id permute_fields) v.lorentz;
	color = List.map (Color.map_tensor permute_fields) v.color }

    let permutations v =
      List.map (permute v)
	(Combinatorics.permute (ThoList.range 0 (Array.length v.fields - 1)))
            
    let write_fusion v =
      match Array.to_list v.fields with
      | lhs :: rhs ->
	Printf.printf "! FUSION: %s <- %s\n" lhs (String.concat " + " rhs);
	()
      | [] -> ()

    let write_fusions v =
      List.iter write_fusion (permutations v)

(* Testing: *)

    let mu = 0

    let i0 = Field.of_int 0
    and i1 = Field.of_int 1
    and i2 = Field.of_int 2
    and i3 = Field.of_int 3

    let mu = Lorentz.Index.of_int 0
    and nu = Lorentz.Index.of_int 1

    let vector_current =
      { fields = [| "tbar"; "gl"; "t" |];
	lorentz = [ (Lorentz.Integer 1, [Lorentz.V (mu, i0, i2)]) ];
	color = [ (Color.Integer 1, [Color.T (i1, i0, i2)])] }

    let vector_current_out_of_bounds =
      { fields = [| "tbar"; "gl"; "t" |];
	lorentz = [ (Lorentz.Integer 1, [Lorentz.V (mu, i3, i2)]) ];
	color = [ (Color.Integer 1, [Color.T (i1, i0, i2)])] }

    let vector_current_color_mismatch =
      { fields = [| "t"; "gl"; "t" |];
	lorentz = [ (Lorentz.Integer 1, [Lorentz.V (mu, i3, i2)]) ];
	color = [ (Color.Integer 1, [Color.T (i1, i0, i2)])] }

    let anomalous_couplings =
      { fields = [| "W+"; "W-"; "Z"; "Z" |];
	lorentz = [ (Lorentz.Integer 1, [ Lorentz.P (i1, i0);
					  Lorentz.P (i1, i1) ]) ];
	color = [ ] }
      
    exception Inconsistent_vertex

    let example () =
      if not (ok vector_current) then begin
	raise Inconsistent_vertex
      end;
      write_fusions vector_current

    open OUnit

    let vertex_indices_ok =
      "indices/ok" >::
	(fun () ->
	  List.iter
	    (fun v ->
	      assert_bool "vector_current" (ok v))
	    (permutations vector_current))
		
    let vertex_indices_broken =
      "indices/broken" >::
	(fun () ->
	  assert_bool "out of bounds"
	    (not (ok vector_current_out_of_bounds));
	  assert_bool "color mismatch"
	    (not (ok vector_current_color_mismatch)))
		
    let anomalous_couplings_ok =
      "anomalous_couplings/ok" >::
	(fun () ->
	  assert_bool "anomalous couplings"
	    (ok anomalous_couplings))
		
    let suite =
      "Vertex" >:::
	[vertex_indices_ok;
	 vertex_indices_broken;
	 anomalous_couplings_ok]
      
  end

(*i ********************************************************************

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

******************************************************************** i*)

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  compile-command:"ocamlc -o vertex thoList.ml{i,} pmap.ml{i,} vertex.ml"
 *  End:
i*)
