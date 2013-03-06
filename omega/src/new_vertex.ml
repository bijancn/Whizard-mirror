module type Test =
  sig
    val suite : OUnit.test
    val time : unit -> unit
  end

let time f x =
  let start = Sys.time () in
  let f_x = f x in
  let stop = Sys.time () in
  (f_x, stop -. start)
  
let print_time msg f x =
  let f_x, seconds = time f x in
  Printf.printf "%s took %10.2f ms\n" msg (seconds *. 1000.);
  f_x
  
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

    let time () =
      ()

  end

module type Permutation =
  sig
    type t
    val of_list : int list -> t
    val of_array : int array -> t
    val inverse : t -> t
    val compose : t -> t -> t
    val list : t -> 'a list -> 'a list
    val array : t -> 'a array -> 'a array
  end

module Permutation_List : Permutation =
  struct

    type t = int list

    let of_list p =
      if List.sort compare p <> (ThoList.range 0 (List.length p - 1)) then
	invalid_arg "Permutation.of_list"
      else
	p

    let of_array p =
      try
	of_list (Array.to_list p)
      with 
      | Invalid_argument "Permutation.of_list" ->
	invalid_arg "Permutation.of_array"

    let inverse p = snd (ThoList.ariadne_sort p)

    let list p l =
      List.map snd
	(List.sort compare
	   (try
	      List.rev_map2 (fun i x -> (i, x)) p l
	    with
	    | Invalid_argument "List.rev_map2" ->
	      invalid_arg "Permutation.list: length mismatch"))

    let array p a =
      try
	Array.of_list (list p (Array.to_list a))
      with 
      | Invalid_argument "Permutation.list: length mismatch" ->
	invalid_arg "Permutation.array: length mismatch"

(* Probably not optimal (or really inefficient), but correct by
   associativity. *)

    let compose p q =
      list (inverse q) p

  end

module Permutation_Array : Permutation =
  struct

    type t = int array

    let of_list p =
      if List.sort compare p <> (ThoList.range 0 (List.length p - 1)) then
	invalid_arg "Permutation.of_list"
      else
	Array.of_list p

    let of_array p =
      try
	of_list (Array.to_list p)
      with 
      | Invalid_argument "Permutation.of_list" ->
	invalid_arg "Permutation.of_array"
      
      let inverse p =
      let len_p = Array.length p in
      let p' = Array.make len_p p.(0) in
      for i = 0 to pred len_p do
	p'.(p.(i)) <- i
      done;
      p'

    let array p a =
      let len_a = Array.length a
      and len_p = Array.length p in
      if len_a <> len_p then
	invalid_arg "Permutation.array: length mismatch";
      let a' = Array.make len_a a.(0) in
      for i = 0 to pred len_a do
	a'.(p.(i)) <- a.(i)
      done;
      a'

    let list p l =
      try
	Array.to_list (array p (Array.of_list l))
      with 
      | Invalid_argument "Permutation.array: length mismatch" ->
	invalid_arg "Permutation.list: length mismatch"

    let compose p q =
      array (inverse q) p

  end

module Permutation = Permutation_Array

(* To shuffle an array a of n elements (indices 0..n-1):

     for i from n − 1 downto 1 do
          j ← random integer with 0 ≤ j ≤ i
          exchange a[j] and a[i]

   To initialize an array a of n elements to a randomly shuffled copy
   of source, both 0-based: 

     a[0] ← source[0]
     for i from 1 to n − 1 do
         j ← random integer with 0 ≤ j ≤ i
         a[i] ← a[j]
         a[j] ← source[i] *)

let shuffle l =
  let a = Array.of_list l in
  for n = Array.length a - 1 downto 1 do
    let k = Random.int (succ n) in
    if k <> n then
      let tmp  = Array.get a n in
      Array.set a n (Array.get a k);
      Array.set a k tmp
  done;
  Array.to_list a

module Permutation_Test (Permutation : Permutation) : Test =
  struct

    open OUnit
    module P = Permutation
    open P

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
	    (Invalid_argument "Permutation.list: length mismatch")
	    (fun () ->
	      list (of_list [0;1;2;3;4]) [0;1;2;3]))

    let apply_ok =
      "ok" >::
	(fun () ->
	  assert_equal [2;0;1;3;5;4]
	    (list (of_list [1;2;0;3;5;4]) [0;1;2;3;4;5]))

    let suite_apply =
      "apply" >:::
	[apply_invalid_lengths;
	 apply_ok]

    let inverse_ok =
      "ok" >::
	(fun () ->
	  let l = shuffle (ThoList.range 0 1000) in
	  let p = of_list (shuffle l) in
	  assert_equal l (list (inverse p) (list p l)))

    let suite_inverse =
      "inverse" >:::
	[inverse_ok]

    let compose_ok =
      "ok" >::
	(fun () ->
	  let id = ThoList.range 0 1000 in
	  let p = of_list (shuffle id)
	  and q = of_list (shuffle id)
	  and l = id in
	  assert_equal (list p (list q l)) (list (compose p q) l))
		
    let compose_inverse_ok =
      "inverse/ok" >::
	(fun () ->
	  let id = ThoList.range 0 1000 in
	  let p = of_list (shuffle id)
	  and q = of_list (shuffle id) in
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

    let repeat repetitions size =
      let id = ThoList.range 0 size in
      let p = of_list (shuffle id)
      and l = shuffle id in
      print_time (Printf.sprintf "reps=%d, len=%d" repetitions size)
	(fun () ->
	  for i = 1 to repetitions do
	    Permutation.list p l
	  done)
	()
      
    let time () =
      repeat 100000 10;
      repeat 10000 100;
      repeat 1000 1000;
      repeat 100 10000;
      repeat 10 100000;
      ()

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

module Permutation_List_Test = Permutation_Test (Permutation_List)
module Permutation_Array_Test = Permutation_Test (Permutation_Array)

let () =
  let my_name = Sys.argv.(0) in
  let test = ref false
  and timing = ref false
  and verbose = ref false
  and usage = "usage: " ^ my_name ^ " ..." in
  Arg.parse
    [ ("-test", Arg.Set test, "");
      ("-timing", Arg.Set timing, "");
      ("-verbose", Arg.Set verbose, "") ]
    (fun s -> raise (Arg.Bad s))
    usage;
  if !test then begin
    let suite =
      OUnit.(>:::) "All" 
	[Partial_Test.suite;
	 Permutation_List_Test.suite;
	 Permutation_Array_Test.suite] in
    ignore (OUnit.run_test_tt ~verbose:!verbose suite)
  end;
  if !timing then begin
    Partial_Test.time ();
    print_endline "List based:";
    Permutation_List_Test.time ();
    print_endline "Array based:";
    Permutation_Array_Test.time ()
  end;
  ()
