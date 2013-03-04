module type Test =
  sig
    val suite : OUnit.test
  end
    
module type Partial =
  sig
    type domain
    type 'a t
    val of_list : (domain * 'a) list -> 'a t
    val of_lists : domain list -> 'a list -> 'a t
    val apply : 'a t -> domain -> 'a
  end

module Partial (D : Map.OrderedType) : Partial with type domain = D.t =
  struct

    module M = Map.Make (D)

    type domain = D.t
    type 'a t = 'a M.t

    let of_list l =
      List.fold_left (fun m (d, v) -> M.add d v m) M.empty l

    let of_lists domain values =
      of_list
	(try
	   List.rev_map2 (fun d v -> (d, v)) domain values
	 with
	    | Invalid_argument "List.rev_map2" ->
	      invalid_arg "Partial.of_lists: length mismatch")

    let apply partial d = M.find d partial

  end

module Partial_Test : Test=
  struct

    open OUnit

    module P = Partial (struct type t = int let compare = compare end)

    let apply_ok =
      "apply/ok" >::
	(fun () ->
	  let p = P.of_list [ (0,"a"); (1,"b"); (2,"c") ]
	  and l = [ 0; 1; 2 ] in
	  assert_equal [ "a"; "b"; "c" ] (List.map (P.apply p) l))
	
    let suite_apply =
      "apply" >:::
	[apply_ok]

    let suite =
      "Partial" >:::
	[suite_apply]
	
  end

module type Permutation =
  sig
    type t
    val of_list : int list -> t
    val inverse : t -> t
    val compose : t -> t -> t
    val apply : t -> 'a list -> 'a list
  end

module Permutation : Permutation =
  struct

    type t = int list

    let of_list p =
      if List.sort compare p <> (ThoList.range 0 (List.length p - 1)) then
	invalid_arg "Permutation.of_list"
      else
	p

    let inverse p = snd (ThoList.ariadne_sort p)

    let apply pl l =
      List.map snd
	(List.sort compare
	   (try
	      List.rev_map2 (fun p a -> (p, a)) pl l
	    with
	    | Invalid_argument "List.rev_map2" ->
	      invalid_arg "Permutation.apply: length mismatch"))

(* Probably not optimal (or really inefficient), but correct by
   associativity. *)

    let compose p q =
      apply (inverse q) p

(* TODO: steal BatRandom.shuffle to implement random permutations. *)
  end

module Permutation_Test : Test =
  struct

    open OUnit
    open Permutation

    let of_list_overlap =
      "overlap" >::
	(fun () ->
	  assert_raises (Invalid_argument "Permutation.of_list")
	    (fun () ->
	      of_list [0;1;2;2]))
	
    let of_list_gap =
      "gap" >::
	(fun () ->
	  assert_raises (Invalid_argument "Permutation.of_list")
	    (fun () ->
	      of_list [0;1;2;4;5]))

    let of_list_ok =
      "ok" >::
	(fun () ->
	  let l = ThoList.range 0 10 in
	  assert_equal (of_list l) (of_list l))

    let suite_of_list =
      "of_list" >:::
	[of_list_overlap;
	 of_list_gap;
	 of_list_ok]

    let apply_invalid_lengths =
      "invalid/lengths" >::
	(fun () ->
	  assert_raises
	    (Invalid_argument "Permutation.apply: length mismatch")
	    (fun () ->
	      apply (of_list [0;1;2;3;4]) [0;1;2;3]))

    let apply_ok =
      "ok" >::
	(fun () ->
	  assert_equal [2;0;1;3;5;4]
	    (apply (of_list [1;2;0;3;5;4]) [0;1;2;3;4;5]))

    let suite_apply =
      "apply" >:::
	[apply_invalid_lengths;
	 apply_ok]

    let inverse_ok =
      "ok" >::
	(fun () ->
	  let p = of_list [1;2;0;3;5;4]
	  and l = [0;1;2;3;4;5] in
	  assert_equal l (apply (inverse p) (apply p l)))

    let suite_inverse =
      "inverse" >:::
	[inverse_ok]

    let compose_ok =
      "ok" >::
	(fun () ->
	  let p = of_list [1;2;0]
	  and q = of_list [0;2;1]
	  and l = [0;1;2] in
	  assert_equal (apply p (apply q l)) (apply (compose p q) l))
		
    let compose_inverse_ok =
      "inverse/ok" >::
	(fun () ->
	  let p = of_list [1;2;0]
	  and q = of_list [0;2;1] in
	  assert_equal
	    (compose (inverse p) (inverse q))
	    (inverse (compose q p)))
		
    let suite_compose =
      "compose" >:::
	[compose_ok;
	 compose_inverse_ok]

    let suite =
      "Permutations" >:::
	[suite_of_list;
	 suite_apply;
	 suite_inverse;
	 suite_compose]

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
  let suite =
    OUnit.(>:::) "All" 
      [Partial_Test.suite;
       Permutation_Test.suite] in
  let _ =
    OUnit.run_test_tt ~verbose:!verbose suite in
  ()
