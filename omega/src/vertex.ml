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

let distinct2 i j =
  i <> j

let distinct3 i j k =
  i <> j && j <> k && k <> i

let distinct ilist =
  List.length (ThoList.uniq (List.sort compare ilist)) =
  List.length ilist

(* An abstract type that allows us to distinguish offsets
   in the field array from color and Lorentz indices in
   different representations. *)

module type Index =
  sig
    type t
    val of_int : int -> t
    val to_int : t -> int
  end

module type Field =
  sig
    type t
    exception Out_of_range of int
    val of_int : context -> int -> t
    val to_int : t -> int
    val get : 'a array -> t -> 'a
  end

module Field : Field =
  struct
    type t = int
    exception Out_of_range of int
    let of_int context i =
      if 0 <= i && i < context.arity then
        i
      else
        raise (Out_of_range i)
    let to_int i = 0
    let get = Array.get
  end

type field = Field.t

module type Lorentz =
  sig
  end

module Lorentz (* : Lorentz *) =
  struct

    
    type index =
      | I of int (* $\mu_0,\mu_1,\ldots$, not $0,1,2,3$ *)
      | F of field

    let map_index fi ff = function
      | I i -> I (fi i)
      | F i -> F (ff i)

    type vector = Vector of index
    type spinor = Spinor of index
    type conjspinor = ConjSpinor of index

    let map_vector fi ff (Vector i) = Vector (map_index fi ff i)
    let map_spinor fi ff (Spinor i) = Spinor (map_index fi ff i)
    let map_conjspinor fi ff (ConjSpinor i) = ConjSpinor (map_index fi ff i)

    let vector_ok context = function
      | Vector (I _) -> true
      | Vector (F i) ->
          begin
            match Field.get context.lorentz_reps i with
            | Coupling.Vector -> true
            | Coupling.Vectorspinor ->
	        failwith "Lorentz.vector_ok: incomplete"
            | _ -> false
          end
      
    let spinor_ok context = function
      | Spinor (I _) -> true
      | Spinor (F i) ->
          begin
            match Field.get context.lorentz_reps i with
            | Coupling.Spinor -> true
            | Coupling.Vectorspinor | Coupling.Majorana ->
	        failwith "Lorentz.spinor_ok: incomplete"
            | _ -> false
          end

    let conjspinor_ok context = function
      | ConjSpinor (I _) -> true
      | ConjSpinor (F i) ->
          begin
            match Field.get context.lorentz_reps i with
            | Coupling.ConjSpinor -> true
            | Coupling.Vectorspinor | Coupling.Majorana ->
	        failwith "Lorentz.conjspinor_ok: incomplete"
            | _ -> false
          end

    let spinor_sandwitch_ok context i j =
      (* [distinct2 i j] only guaranteed for Dirac spinors! *)
      conjspinor_ok context i && spinor_ok context j

    type primitive =
      | G of vector * vector
      | E of vector * vector * vector * vector
      | K of vector * field
      | S of conjspinor * spinor
      | V of vector * conjspinor * spinor
      | T of vector * vector * conjspinor * spinor
      | A of vector * conjspinor * spinor
      | P of conjspinor * spinor

    let map_primitive fvi fvf fsi fsf fci fcf = function
      | G (mu, nu) ->
          G (map_vector fvi fvf mu, map_vector fvi fvf nu)
      | E (mu, nu, rho, sigma) ->
          E (map_vector fvi fvf mu,
             map_vector fvi fvf nu,
             map_vector fvi fvf rho,
             map_vector fvi fvf sigma)
      | K (mu, i) ->
          K (map_vector fvi fvf mu, fvf i)
      | S (i, j) ->
          S (map_conjspinor fci fcf i, map_spinor fsi fsf j)
      | V (mu, i, j) ->
          V (map_vector fvi fvf mu,
             map_conjspinor fci fcf i,
             map_spinor fsi fsf j)
      | T (mu, nu, i, j) ->
          T (map_vector fvi fvf mu,
             map_vector fvi fvf nu,
             map_conjspinor fci fcf i,
             map_spinor fsi fsf j)
      | A (mu, i, j) ->
          A (map_vector fvi fvf mu,
             map_conjspinor fci fcf i,
             map_spinor fsi fsf j)
      | P (i, j) ->
          P (map_conjspinor fci fcf i, map_spinor fsi fsf j)

    let primitive_ok context =
      function
	| G (mu, nu) ->
            distinct2 mu nu &&
            vector_ok context mu && vector_ok context nu
        | E (mu, nu, rho, sigma) ->
            let i = [mu; nu; rho; sigma] in
            distinct i && List.for_all (vector_ok context) i
	| K (mu, i) ->
            vector_ok context mu
	| S (i, j) | P (i, j) ->
            spinor_sandwitch_ok context i j
        | V (mu, i, j) | A (mu, i, j) ->
            vector_ok context mu && spinor_sandwitch_ok context i j
	| T (mu, nu, i, j) ->
            vector_ok context mu && vector_ok context nu &&
            spinor_sandwitch_ok context i j

    let idx (Vector v) =
      match v with
      | I i -> [i]
      | F _ -> []

    let primitive_indices = function
      | G (mu, nu) | T (mu, nu, _, _) -> idx mu @ idx nu
      | E (mu, nu, rho, sigma) -> idx mu @ idx nu @ idx rho @ idx sigma
      | K (mu, _) | V (mu, _, _) | A (mu, _, _) -> idx mu
      | S (_, _) | P (_, _) -> []

    let indices p =
      ThoList.flatmap primitive_indices p

    let contraction_ok p =
      let c = ThoList.classify (indices p) in
      print_endline
        (String.concat ", "
           (List.map
              (fun (n, i) -> string_of_int n ^ " * " ^ string_of_int i)
              c));
      flush stdout;
      let res = List.for_all (fun (n, _) -> n = 2) c in
      res

    let contraction_ok p =
      List.for_all
	(fun (n, _) -> n = 2)
	(ThoList.classify (indices p))

    type tensor = int * primitive list

    let map_tensor fvi fvf fsi fsf fci fcf (factor, primitives) =
      (factor, List.map (map_primitive fvi fvf fsi fsf fci fcf ) primitives)

    let tensor_ok context (_, primitives) =
      List.for_all (primitive_ok context) primitives &&
      contraction_ok primitives

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

    (* $a_0,a_1,\ldots$, not $0,1,\ldots$ *)
    type index = Index.t

    type color_rep =
      | F of field
      | C of field
      | A of field

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

    let primitive_ok ctx =
      function
	| D (i, j) ->
	    distinct2 i j &&
	    (match Field.get ctx.color_reps i, Field.get ctx.color_reps j with
	    | Color.SUN (n1), Color.SUN (n2) ->
		n1 = - n2 && n2 > 0
	    | _, _ -> false)
	| E (i, j, k) ->
	    distinct3 i j k &&
	    (match Field.get ctx.color_reps i,
	      Field.get ctx.color_reps j, Field.get ctx.color_reps k with
	    | Color.SUN (n1), Color.SUN (n2), Color.SUN (n3) ->
		n1 = 3 && n2 = 3 && n3 = 3 ||
		n1 = -3 && n2 = -3 && n3 = -3
	      | _, _, _ -> false)
	| T (a, i, j) ->
	    distinct3 a i j &&
	    (match Field.get ctx.color_reps a,
	      Field.get ctx.color_reps i, Field.get ctx.color_reps j with
	    | Color.AdjSUN(n1), Color.SUN (n2), Color.SUN (n3) ->
		n1 = n3 && n2 = - n3 && n3 > 0
	    | _, _, _ -> false)
	| F (a, b, c) ->
	    distinct3 a b c &&
	    (match Field.get ctx.color_reps a,
              Field.get ctx.color_reps b, Field.get ctx.color_reps c with
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
      List.for_all
	(fun (n, _) -> n = 2)
	(ThoList.classify (indices p))

    type tensor = int * primitive list

    let map_tensor f (factor, primitives) =
      (factor, List.map (map_primitive f) primitives)

    let tensor_ok context (_, primitives) =
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

    let context_of_flavors flavors =
      { arity = Array.length flavors;
	lorentz_reps = Array.map M.lorentz flavors;
	color_reps = Array.map M.color flavors }

    let context_of_flavor_names names =
      context_of_flavors (Array.map M.flavor_of_string names)

    let context_of_vertex v =
      context_of_flavor_names v.fields

    let ok v =
      let context = context_of_vertex v in
      List.for_all (Lorentz.tensor_ok context) v.lorentz &&
	List.for_all (Color.tensor_ok context) v.color

    module PM =
      Partial.Make (struct type t = field let compare = compare end)

    let id x = x

    let permute v p =
      let context = context_of_vertex v in
      let sorted =
        List.map
          (Field.of_int context)
          (ThoList.range 0 (Array.length v.fields - 1)) in
      let permute =
        PM.apply (PM.of_lists sorted (List.map (Field.of_int context) p)) in
      { fields = Permutation.array (Permutation.of_list p) v.fields;
 	lorentz = List.map
          (Lorentz.map_tensor id permute id permute id permute) v.lorentz;
	color = List.map (Color.map_tensor permute) v.color }

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

    let vector_field context i =
      Lorentz.Vector (Lorentz.F (Field.of_int context i))

    let spinor_field context i =
      Lorentz.Spinor (Lorentz.F (Field.of_int context i))

    let conjspinor_field context i =
      Lorentz.ConjSpinor (Lorentz.F (Field.of_int context i))

    let mu = Lorentz.Vector (Lorentz.I 0)
    and nu = Lorentz.Vector (Lorentz.I 1)

    let tbar_gl_t = [| "tbar"; "gl"; "t" |]
    let context = context_of_flavor_names tbar_gl_t
     
    let vector_current_ok =
      { fields = tbar_gl_t;
	lorentz = [ (1, [Lorentz.V (vector_field context 1,
                                    conjspinor_field context 0,
                                    spinor_field context 2)]) ];
	color = [ (1, [Color.T (Field.of_int context 1,
                                Field.of_int context 0,
                                Field.of_int context 2)])] }

    let vector_current_vector_misplaced =
      { fields = tbar_gl_t;
	lorentz = [ (1, [Lorentz.V (vector_field context 2,
                                    conjspinor_field context 0,
                                    spinor_field context 2)]) ];
	color = [ (1, [Color.T (Field.of_int context 1,
                                Field.of_int context 0,
                                Field.of_int context 2)])] }

    let vector_current_spinor_misplaced =
      { fields = tbar_gl_t;
	lorentz = [ (1, [Lorentz.V (vector_field context 1,
                                    conjspinor_field context 0,
                                    spinor_field context 1)]) ];
	color = [ (1, [Color.T (Field.of_int context 1,
                                Field.of_int context 0,
                                Field.of_int context 2)])] }

    let vector_current_conjspinor_misplaced =
      { fields = tbar_gl_t;
	lorentz = [ (1, [Lorentz.V (vector_field context 1,
                                    conjspinor_field context 1,
                                    spinor_field context 2)]) ];
	color = [ (1, [Color.T (Field.of_int context 1,
                                Field.of_int context 0,
                                Field.of_int context 2)])] }

    let vector_current_out_of_bounds () =
      { fields = tbar_gl_t;
	lorentz = [ (1, [Lorentz.V (mu,
                                    conjspinor_field context 3,
                                    spinor_field context 2)]) ];
	color = [ (1, [Color.T (Field.of_int context 1,
                                Field.of_int context 0,
                                Field.of_int context 2)])] }

    let vector_current_color_mismatch =
      let names = [| "t"; "gl"; "t" |] in
      let context = context_of_flavor_names names in
      { fields = names;
	lorentz = [ (1, [Lorentz.V (mu,
                                    conjspinor_field context 0,
                                    spinor_field context 2)]) ];
	color = [ (1, [Color.T (Field.of_int context 1,
                                Field.of_int context 0,
                                Field.of_int context 2)])] }

    let wwzz = [| "W+"; "W-"; "Z"; "Z" |]
    let context = context_of_flavor_names wwzz

    let anomalous_couplings =
      { fields = wwzz;
	lorentz = [ (1, [ Lorentz.K (mu, Field.of_int context 0);
                          Lorentz.K (mu, Field.of_int context 1) ]) ];
	color = [ ] }
      
    let anomalous_couplings_index_mismatch =
      { fields = wwzz;
	lorentz = [ (1, [ Lorentz.K (mu, Field.of_int context 0);
                          Lorentz.K (nu, Field.of_int context 1) ]) ];
	color = [ ] }
      
    exception Inconsistent_vertex

    let example () =
      if not (ok vector_current_ok) then begin
	raise Inconsistent_vertex
      end;
      write_fusions vector_current_ok

    open OUnit

    let vertex_indices_ok =
      "indices/ok" >::
	(fun () ->
	  List.iter
	    (fun v ->
	      assert_bool "vector_current" (ok v))
	    (permutations vector_current_ok))
		
    let vertex_indices_broken =
      "indices/broken" >::
	(fun () ->
	  assert_bool "vector misplaced"
	    (not (ok vector_current_vector_misplaced));
	  assert_bool "conjugate spinor misplaced"
	    (not (ok vector_current_spinor_misplaced));
	  assert_bool "conjugate spinor misplaced"
	    (not (ok vector_current_conjspinor_misplaced));
          assert_raises (Field.Out_of_range 3)
	    vector_current_out_of_bounds;
	  assert_bool "color mismatch"
	    (not (ok vector_current_color_mismatch)))
		
    let anomalous_couplings_ok =
      "anomalous_couplings/ok" >::
	(fun () ->
	  assert_bool "anomalous couplings"
	    (ok anomalous_couplings))
		
    let anomalous_couplings_broken =
      "anomalous_couplings/broken" >::
	(fun () ->
	  assert_bool "anomalous couplings"
	    (not (ok anomalous_couplings_index_mismatch)))
		
    let suite =
      "Vertex" >:::
	[vertex_indices_ok;
	 vertex_indices_broken;
	 anomalous_couplings_ok;
         anomalous_couplings_broken]
      
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

(*
let tgv = parse
    "(k1.e3 - k2.e3)*e1.e2 + (k2.e1 - k3.e1)*e2.e3 + (k3.e2 - k1.e2)*e3.e1"

let tgv = parse
    "(k1 - k2).e3*e1.e2 + (k2 - k3).e1*e2.e3 + (k3 - k1).e2*e3.e1"
 *)

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

(* module IMap = Map.Make (struct type t = int let compare = compare end) *)

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

(*
let _ =
  process_vertex (parse (read_line ()))
 *)

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
