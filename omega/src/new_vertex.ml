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

let inverse_permutation l = snd (ThoList.ariadne_sort l)
let apply_permutation pl l =
  List.map snd
    (List.sort
       (fun (p1, _) (p2, _) ->
	 match Pervasives.compare p1 p2 with
	 | 0 -> invalid_arg "apply_permutation: not unique"
	 | n -> n)
       (try
	  List.rev_map2 (fun p a -> (p, a)) pl l
	with
	| Invalid_argument "List.rev_map2" ->
	  invalid_arg "apply_permutation: wrong length"))
	
let vector_current =
  { fields = [| Coupling.ConjSpinor; Coupling.Vector; Coupling.Spinor |];
    lorentz = [ (Integer 1, [Gamma (0, 2)]) ];
    color = [ (1, [T (1, 0, 2)])] }

open OUnit


let test_apply_permutation_invalid_lengths =
  "apply_permutation/invalid/lengths" >::
    (fun () ->
      assert_raises (Invalid_argument "apply_permutation: wrong length")
	(fun () ->
	  apply_permutation [0;1;2;3;4] [0;1;2;3]))

let test_apply_permutation_invalid_permulation =
  "apply_permutation/invalid/permutation" >::
    (fun () ->
      assert_raises (Invalid_argument "apply_permutation: not unique")
	(fun () ->
	  apply_permutation [0;1;2;2] [0;1;2;3]))

let test_apply_permutation_ok =
  "apply_permutation/ok" >::
    (fun () ->
      assert_equal [2;0;1;3;5;4]
	(apply_permutation [1;2;0;3;5;4] [0;1;2;3;4;5]))

let suite_permutations =
  "permutations" >:::
    [test_apply_permutation_invalid_lengths;
     test_apply_permutation_invalid_permulation;
     test_apply_permutation_ok]


let () =
  let my_name = Sys.argv.(0) in
  let verbose = ref false
  and usage = "usage: " ^ my_name ^ " ..." in
  Arg.parse
    [ ("-verbose", Arg.Set verbose, "") ]
    (fun s -> raise (Arg.Bad s))
    usage;
  let _ = run_test_tt ~verbose:!verbose suite_permutations in
  ()
