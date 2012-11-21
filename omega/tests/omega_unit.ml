(* $Id: omega_unit.ml 4003 2012-11-20 22:03:43Z ohl $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
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

open OUnit

let unattended = ref true

let skip_if_unattended () =
  skip_if !unattended "not suitable for unattended tests"

let trivial_test =
  "trivial" >::
    (bracket
       (fun () -> true)
       (fun b -> assert_bool "always true" b)
       (fun b -> ()))

let short_random_list n =
  let l = ref [] in
  for i = 1 to n do
    l := Random.int 1024 :: !l
  done;
  !l

let allowed_recursion_depth () =
  let rec allowed_recursion_depth' n =
    try
      allowed_recursion_depth' (succ n)
    with
    | Stack_overflow -> n in
  allowed_recursion_depth' 0

let long_random_list factor =
  let n = factor * allowed_recursion_depth () in
  let l = ref [] in
  for i = 1 to n do
    l := Random.int n :: !l
  done;
  !l

module Integer = 
  struct 
    type t = int
    let compare = compare
    let pp_printer = Format.pp_print_int
    let pp_print_sep = OUnitDiff.pp_comma_separator
  end

module Integer_List = OUnitDiff.ListSimpleMake(Integer)

module ThoList_Unit_Tests =
  struct

    let inner_list = ThoList.range 1 5
    let outer_list = List.map (( * ) 10) (ThoList.range 1 4)
    let f n = List.map ((+) n) inner_list
      
    let flatmap =
      "flatmap" >::
	(fun () ->
	  let result = ThoList.flatmap f outer_list
	  and expected = List.flatten (List.map f outer_list) in
	  assert_equal expected result)

    let rev_flatmap =
      "rev_flatmap" >::
	(fun () ->
	  let result = ThoList.rev_flatmap f outer_list
	  and expected = List.rev (ThoList.flatmap f outer_list) in
	  Integer_List.assert_equal expected result)

    let flatmap_stack_overflow =
      "flatmap_stack_overflow" >::
	(fun () ->
	  skip_if !unattended "memory limits not suitable for unattended tests";
	  let l = long_random_list 2 in
	  let f n = List.map ((+) n) (short_random_list 2) in
	  assert_raises Stack_overflow
	    (fun () -> ThoList.flatmap f l))

    let rev_flatmap_no_stack_overflow =
      "rev_flatmap_no_stack_overflow" >::
	(fun () ->
	  skip_if !unattended "memory limits not suitable for unattended tests";
	  let l = long_random_list 10 in
	  let f n = List.map ((+) n) (short_random_list 10) in
	  ignore (ThoList.rev_flatmap f l);
	  assert_bool "always true" true)

    let suite =
      "ThoList" >:::
	[flatmap;
	 flatmap_stack_overflow;
	 rev_flatmap;
	 rev_flatmap_no_stack_overflow ]

  end

module IListSet =
  Set.Make (struct type t = int list let compare = compare end)

let list_elements_unique l =
  let rec list_elements_unique' set = function
    | [] -> true
    | x :: rest ->
      if IListSet.mem x set then
	false
      else
	list_elements_unique' (IListSet.add x set) rest in
  list_elements_unique' IListSet.empty l

let ilistset_test =
  "IListSet" >::
    (fun () ->
      assert_bool "true" (list_elements_unique [[1];[2]]);
      assert_bool "false" (not (list_elements_unique [[1];[1]])))

module Combinatorics_Unit_Tests =
  struct

    let permute =
      "permute" >::
	(fun () ->
	  let n = 7 in
	  let l = ThoList.range 1 n in
	  let result = Combinatorics.permute l in
	  assert_equal (Combinatorics.factorial n) (List.length result);
	  assert_bool "unique" (list_elements_unique result))

    let suite =
      "Combinatorics" >:::
	[permute]

  end

let selftest_suite =
  "testsuite" >:::
    [trivial_test;
     ilistset_test]

let suite = 
  "omega" >:::
    [selftest_suite;
     ThoList_Unit_Tests.suite; 
     Combinatorics_Unit_Tests.suite]

let _ =
  ignore
    (run_test_tt_main
       ~arg_specs:[("-attended", Arg.Clear unattended,
		    "      run tests that depend on the environment");
		   ("-unattended", Arg.Set unattended,
		    "    don't run tests depend on the environment")]
       suite);
  exit 0
