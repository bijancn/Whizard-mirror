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

let trivial_test =
  "trivial" >::
    (bracket
       (fun () -> true)
       (fun b -> assert_bool "always true" b)
       (fun b -> ()))

let random_list n =
  let l = ref [] in
  for i = 1 to n do
    l := Random.int 1024 :: !l
  done;
  !l

let flatmap_test =
  "flatmap" >::
    (fun () ->
      let inner_list = random_list 20 in
      let f n = List.map ((+) n) inner_list
      and outer_list = random_list 30 in
      let my = ThoList.flatmap f outer_list
      and std = List.concat (List.map f outer_list) in
      assert_equal my std)

let flatmap'_test =
  "flatmap'" >::
    (fun () ->
      let inner_list = random_list 20 in
      let f n = List.map ((+) n) inner_list
      and f' n = List.rev_map ((+) n) inner_list
      and outer_list = random_list 30 in
      let my = ThoList.flatmap f outer_list
      and std = List.rev (List.concat (List.rev_map f' outer_list)) in
      assert_equal my std)

let tholist_suite =
  "ThoList" >:::
    [flatmap_test;
     flatmap'_test]

let suite = 
  "O'Mega Unit Tests" >:::
    [trivial_test;
     tholist_suite]

let _ = 
  run_test_tt_main suite
