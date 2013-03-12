(* $Id: omega_unit.ml 4015 2013-01-03 16:04:18Z jr_reuter $

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

(* open Vertex *)

module Permutation_List_Test = Vertex.Permutation_Test (Permutation.Using_Lists)
module Permutation_Array_Test = Vertex.Permutation_Test (Permutation.Using_Arrays)

module Vertex_Test =
  Vertex.Make_Vertex_Test (Modellib_SM.SM(Modellib_SM.SM_no_anomalous))

let _ =
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
	[Partial.Test.suite;
	 Permutation_List_Test.suite;
	 Permutation_Array_Test.suite;
	 Vertex_Test.test_suite] in
    ignore (OUnit.run_test_tt ~verbose:!verbose suite)
  end;
  if !timing then begin
    Partial.Test.time ();
    print_endline "List based:";
    Permutation_List_Test.time ();
    print_endline "Array based:";
    Permutation_Array_Test.time ()
  end;
  Vertex_Test.example ();
  exit 0
