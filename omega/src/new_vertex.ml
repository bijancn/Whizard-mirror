module type Test =
  sig
    val suite : OUnit.test
  end
    
module type Permutation =
  sig
    type t
    val inverse : t -> t
    val apply : t -> 'a list -> 'a list
    module Test : Test
  end

module Permutation : Permutation =
  struct

    type t = int list

    let inverse l = snd (ThoList.ariadne_sort l)

    let apply pl l =
      List.map snd
	(List.sort
	   (fun (p1, _) (p2, _) ->
	     match Pervasives.compare p1 p2 with
	     | 0 -> invalid_arg "Permutation.apply: not unique"
	     | n -> n)
	   (try
	      List.rev_map2 (fun p a -> (p, a)) pl l
	    with
	    | Invalid_argument "List.rev_map2" ->
	      invalid_arg "Permutation.apply: wrong length"))

    module Test =
      struct

	open OUnit

	let apply_invalid_lengths =
	  "apply/invalid/lengths" >::
	    (fun () ->
	      assert_raises (Invalid_argument "Permutation.apply: wrong length")
		(fun () ->
		  apply [0;1;2;3;4] [0;1;2;3]))

	let apply_invalid_permulation =
	  "apply/invalid/permutation" >::
	    (fun () ->
	      assert_raises (Invalid_argument "Permutation.apply: not unique")
		(fun () ->
		  apply [0;1;2;2] [0;1;2;3]))

	let apply_ok =
	  "apply/ok" >::
	    (fun () ->
	      assert_equal [2;0;1;3;5;4]
		(apply [1;2;0;3;5;4] [0;1;2;3;4;5]))

	let apply_inverse_ok =
	  "apply/inverse/ok" >::
	    (fun () ->
	      let p = [1;2;0;3;5;4]
	      and l = [0;1;2;3;4;5] in
	      assert_equal l (apply (inverse p) (apply p l)))

	let suite =
	  "Permutations" >:::
	    [apply_invalid_lengths;
	     apply_invalid_permulation;
	     apply_ok;
	     apply_inverse_ok]
      end

  end

type index = int

type momentum = index
type gamma = index * index

type vector =
| Momentum of momentum
| Gamma of gamma
| Gamma5 of gamma

type factor =
| Integer of int
| Contraction of vector * vector

type lorentz_tensor = factor * vector list

type color =
| Fundamental of index
| Conjugate of index
| Adjoint of index

type color_primitive =
| Unit
| Delta of index * index
| T of index * index * index
| F of index * index * index

type color_tensor = int * color_primitive list

type vertex =
  { fields: Coupling.lorentz array;
    lorentz : lorentz_tensor list;
    color : color_tensor list }

let vector_current =
  { fields = [| Coupling.ConjSpinor; Coupling.Vector; Coupling.Spinor |];
    lorentz = [ (Integer 1, [Gamma (0, 2)]) ];
    color = [ (1, [T (1, 0, 2)])] }

let () =
  let my_name = Sys.argv.(0) in
  let verbose = ref false
  and usage = "usage: " ^ my_name ^ " ..." in
  Arg.parse
    [ ("-verbose", Arg.Set verbose, "") ]
    (fun s -> raise (Arg.Bad s))
    usage;
  let _ = OUnit.run_test_tt ~verbose:!verbose Permutation.Test.suite in
  ()
