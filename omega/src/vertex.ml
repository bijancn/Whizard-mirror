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

    let indices = function
      | I i -> [i]
      | F _ -> []

    (* Is the following level of type checks useful or redundant? *)

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

    let primitive_vector_indices = function
      | G (Vector mu, Vector nu) | T (Vector mu, Vector nu, _, _) ->
          indices mu @ indices nu
      | E (Vector mu, Vector nu, Vector rho, Vector sigma) ->
          indices mu @ indices nu @ indices rho @ indices sigma
      | K (Vector mu, _)
      | V (Vector mu, _, _)
      | A (Vector mu, _, _) -> indices mu
      | S (_, _) | P (_, _) -> []

    let vector_indices p =
      ThoList.flatmap primitive_vector_indices p

    let primitive_spinor_indices = function
      | G (_, _) | E (_, _, _, _) | K (_, _) -> []
      | S (_, Spinor alpha) | V (_, _, Spinor alpha)
      | T (_, _, _, Spinor alpha)
      | A (_, _, Spinor alpha) | P (_, Spinor alpha) -> indices alpha

    let spinor_indices p =
      ThoList.flatmap primitive_spinor_indices p

    let primitive_conjspinor_indices = function
      | G (_, _) | E (_, _, _, _) | K (_, _) -> []
      | S (ConjSpinor alpha, _) | V (_, ConjSpinor alpha, _)
      | T (_, _, ConjSpinor alpha, _)
      | A (_, ConjSpinor alpha, _) | P (ConjSpinor alpha, _) -> indices alpha

    let conjspinor_indices p =
      ThoList.flatmap primitive_conjspinor_indices p

    let vector_contraction_ok p =
      let c = ThoList.classify (vector_indices p) in
      print_endline
        (String.concat ", "
           (List.map
              (fun (n, i) -> string_of_int n ^ " * " ^ string_of_int i)
              c));
      flush stdout;
      let res = List.for_all (fun (n, _) -> n = 2) c in
      res

    let vector_contraction_ok p =
      List.for_all
	(fun (n, _) -> n = 2)
	(ThoList.classify (vector_indices p))

    let spinor_contraction_ok p =
      List.for_all
	(fun (n, _) -> n = 2)
	(ThoList.classify (spinor_indices p))

    let conjspinor_contraction_ok p =
      List.for_all
	(fun (n, _) -> n = 2)
	(ThoList.classify (conjspinor_indices p))

    let contraction_ok p =
      vector_contraction_ok p &&
      spinor_contraction_ok p && conjspinor_contraction_ok p

    type tensor = int * primitive list

    let map_tensor fvi fvf fsi fsf fci fcf (factor, primitives) =
      (factor, List.map (map_primitive fvi fvf fsi fsf fci fcf ) primitives)

    let tensor_ok context (_, primitives) =
      List.for_all (primitive_ok context) primitives &&
      contraction_ok primitives

    module Complex =
      struct

        type t = int * int

	type t' = 
	| Z
	| O
	| M
	| I
	| J
	| C of int * int

	let to_fortran = function
	| Z -> "(0,0)"
	| O -> "(1,0)"
	| M -> "(-1,0)"
	| I -> "(0,1)"
	| J -> "(0,-1)"
	| C (r, i) -> "(" ^ string_of_int r ^ "," ^ string_of_int i ^ ")"

      end

    module type Dirac =
      sig
        val scalar : int -> int -> Complex.t
        val vector : int -> int -> int -> Complex.t
        val tensor : int -> int -> int -> int -> Complex.t
        val axial : int -> int -> int -> Complex.t
        val pseudo : int -> int -> Complex.t
      end

    module type Dirac_Matrices =
      sig
        val scalar : (int * int * Complex.t) list
        val vector : (int * int * int * Complex.t) list
        val tensor : (int * int * int * int * Complex.t) list
        val axial : (int * int * int * Complex.t) list
        val pseudo : (int * int * Complex.t) list
      end

    module Chiral : Dirac_Matrices =
      struct

        let scalar =
          [ (1, 1, ( 1,  0));
            (2, 2, ( 1,  0));
            (3, 3, ( 1,  0));
            (4, 4, ( 1,  0)) ]

        let vector =
          [ (0, 1, 4, ( 1,  0));
            (0, 4, 1, ( 1,  0));
            (0, 2, 3, (-1,  0));
            (0, 3, 2, (-1,  0));
            (1, 1, 3, ( 1,  0));
            (1, 3, 1, ( 1,  0));
            (1, 2, 4, (-1,  0));
            (1, 4, 2, (-1,  0));
            (2, 1, 3, ( 0,  1));
            (2, 3, 1, ( 0,  1));
            (2, 2, 4, ( 0,  1));
            (2, 4, 2, ( 0,  1));
            (3, 1, 4, (-1,  0));
            (3, 4, 1, (-1,  0));
            (3, 2, 3, (-1,  0));
            (3, 3, 2, (-1,  0)) ]

        let tensor =
          []

        let axial =
          [ (0, 1, 4, (-1,  0));
            (0, 4, 1, ( 1,  0));
            (0, 2, 3, ( 1,  0));
            (0, 3, 2, (-1,  0));
            (1, 1, 3, (-1,  0));
            (1, 3, 1, ( 1,  0));
            (1, 2, 4, ( 1,  0));
            (1, 4, 2, (-1,  0));
            (2, 1, 3, ( 0, -1));
            (2, 3, 1, ( 0,  1));
            (2, 2, 4, ( 0, -1));
            (2, 4, 2, ( 0,  1));
            (3, 1, 4, ( 1,  0));
            (3, 4, 1, (-1,  0));
            (3, 2, 3, ( 1,  0));
            (3, 3, 2, (-1,  0)) ]

        let pseudo =
          [ (1, 1, (-1,  0));
            (2, 2, (-1,  0));
            (3, 3, ( 1,  0));
            (4, 4, ( 1,  0)) ]

      end

    module Dirac (M : Dirac_Matrices) : Dirac =
      struct

        module Map2 =
          Map.Make
            (struct
              type t = int * int
              let compare = Pervasives.compare
            end)
            
        let init2 triples =
          List.fold_left
            (fun acc (i, j, e) -> Map2.add (i, j) e acc)
            Map2.empty triples

        let bounds_check2 i j =
          if i < 1 or i > 4 or j < 0 or j > 4 then
            invalid_arg "Chiral.bounds_check2"

        let lookup2 map i j =
          bounds_check2 i j;
          try Map2.find (i, j) map with Not_found -> (0, 0)

        module Map3 =
          Map.Make
            (struct
              type t = int * int * int
              let compare = Pervasives.compare
            end)
            
        let init3 quadruples =
          List.fold_left
            (fun acc (mu, i, j, e) -> Map3.add (mu, i, j) e acc)
            Map3.empty quadruples

        let bounds_check3 mu i j =
          bounds_check2 i j;
          if mu < 0 or mu > 3 then
            invalid_arg "Chiral.bounds_check3"

        let lookup3 map mu i j =
          bounds_check3 mu i j;
          try Map3.find (mu, i, j) map with Not_found -> (0, 0)

        module Map4 =
          Map.Make
            (struct
              type t = int * int * int * int
              let compare = Pervasives.compare
            end)
            
        let init4 quadruples =
          List.fold_left
            (fun acc (mu, nu, i, j, e) -> Map4.add (mu, nu, i, j) e acc)
            Map4.empty quadruples

        let bounds_check4 mu nu i j =
          bounds_check3 nu i j;
          if mu < 0 or mu > 3 then
            invalid_arg "Chiral.bounds_check4"

        let lookup4 map mu nu i j =
          bounds_check4 mu nu i j;
          try Map4.find (mu, nu, i, j) map with Not_found -> (0, 0)

        let scalar_map = init2 M.scalar
        let vector_map = init3 M.vector
        let tensor_map = init4 M.tensor
        let axial_map = init3 M.axial
        let pseudo_map = init2 M.pseudo

        let scalar = lookup2 scalar_map
        let vector = lookup3 vector_map
        let tensor mu nu i j =
          failwith "tensor: incomplete";
          lookup4 tensor_map mu nu i j
        let axial = lookup3 axial_map
        let pseudo = lookup2 pseudo_map

      end

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

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

module Test (M : Model.T) : Test =
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

    let wf_declaration flavor =
      match M.lorentz (M.flavor_of_string flavor) with
      | Coupling.Vector -> "vector"
      | Coupling.Spinor -> "spinor"
      | Coupling.ConjSpinor -> "conjspinor"
      | _ -> failwith "wf_declaration: incomplete"

    module Chiral = Lorentz.Dirac(Lorentz.Chiral)

    let write_fusion v =
      match Array.to_list v.fields with
      | lhs :: rhs ->
          let name = lhs ^ "_of_" ^ String.concat "_" rhs in
          let momenta = List.map (fun n -> "k_" ^ n) rhs in
	  Printf.printf "pure function %s (%s) result (%s)\n"
            name (String.concat ", "
                    (List.flatten
                       (List.map2 (fun wf p -> [wf; p]) rhs momenta)))
            lhs;
          Printf.printf "  type(%s) :: %s\n" (wf_declaration lhs) lhs;
          List.iter
            (fun wf ->
              Printf.printf "  type(%s), intent(in) :: %s\n"
                (wf_declaration wf) wf)
            rhs;
          List.iter
            (Printf.printf "  type(momentum), intent(in) :: %s\n")
            momenta;
          let [rhs1; rhs2] = rhs in
          begin match M.lorentz (M.flavor_of_string lhs) with
          | Coupling.Vector ->
              begin
                for mu = 0 to 3 do
                  Printf.printf "  %s(%d) =" lhs mu;
                  for i = 1 to 4 do
                    for j = 1 to 4 do
                      match Chiral.vector mu i j with
                      | 0, 0 -> ()
                      | re, im ->
                          Printf.printf " + (%d,%d)*%s(%d)*%s(%d)"
                            re im rhs1 i rhs2 j
                    done
                  done;
                  Printf.printf "\n"
                done
              end;
          | Coupling.Spinor | Coupling.ConjSpinor ->
              begin
                for i = 1 to 4 do
                  Printf.printf "  %s(%d) =" lhs i;
                  for mu = 0 to 3 do
                    for j = 1 to 4 do
                      match Chiral.vector mu i j with
                      | 0, 0 -> ()
                      | re, im ->
                          Printf.printf " + (%d,%d)*%s(%d)*%s(%d)"
                            re im rhs1 mu rhs2 j
                    done
                  done;
                  Printf.printf "\n"
                done
              end;
          | _ -> failwith "write_fusion: incomplete"
          end;
	  Printf.printf "end function %s\n" name;
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

let parse text =
  try
    Vertex_parser.model Vertex_lexer.token (Lexing.from_string text)
  with
  | Vertex_syntax.Syntax_Error (msg, i, j) ->
      invalid_arg (Printf.sprintf "syntax error (%s) at: `%s'"
                     msg  (String.sub text i (j - i + 1)))
  | Parsing.Parse_error ->
      invalid_arg ("parse error: " ^ text)

module Parser_Test (M : Model.T) : Test =
  struct

    let example () =
      ()

    open OUnit

    (* Hacked ... *)

    module V = Vertex_syntax
    module E = Vertex_syntax.Expr

    let expr =
      "expr" >::
	(fun () ->
	  assert_equal
	    (E.Integer 42,
	     V.List [])
	    (parse "2 * (17 + 4) << >>"))

    let index =
      "index" >::
	(fun () ->
	  assert_equal
	    (E.Integer 1,
	     V.List
	       [ V.Scripted { V.token = V.Name "a";
			      V.super = [V.Digit 2];
			      V.sub = [V.Digit 1] } ] )
	    (parse "<< {a}_{1}^{2} >>"))

    let empty =
      "empty" >::
	(fun () ->
	  assert_equal (E.Integer 1, V.List []) (parse ""))

    let suite =
      "Vertex_Parser" >:::
	[empty;
	 index;
	 expr]

  end
