(* $Id: vertex.ml 7444 2016-02-17 15:37:20Z jr_reuter $

   Copyright (C) 1999-2016 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
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

let error_in_string text start_pos end_pos =
  let i = start_pos.Lexing.pos_cnum
  and j = end_pos.Lexing.pos_cnum in
  String.sub text i (j - i)

let error_in_file name start_pos end_pos =
  Printf.sprintf
    "%s:%d.%d-%d.%d"
    name
    start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

let parse_string text =
  try
    UFO_parser.file
      UFO_lexer.token
      (UFO_lexer.init_position "" (Lexing.from_string text))
  with
  | UFO_syntax.Syntax_Error (msg, start_pos, end_pos) ->
     invalid_arg (Printf.sprintf "syntax error (%s) at: `%s'"
                    msg  (error_in_string text start_pos end_pos))
  | Parsing.Parse_error ->
     invalid_arg ("parse error: " ^ text)

let parse_file name =
  let ic = open_in name in
  let result =
    begin
      try
	UFO_parser.file
	  UFO_lexer.token
	  (UFO_lexer.init_position name (Lexing.from_channel ic))
      with
      | UFO_syntax.Syntax_Error (msg, start_pos, end_pos) ->
	 begin
	   close_in ic;
	   invalid_arg (Printf.sprintf
			  "%s: syntax error (%s)"
			  (error_in_file name start_pos end_pos) msg)
	 end
      | Parsing.Parse_error ->
	 begin
	   close_in ic;
	   invalid_arg ("parse error: " ^ name)
	 end
    end in
  close_in ic;
  result

type files =
  { raw_particles : UFO_syntax.t;
    raw_couplings : UFO_syntax.t;
    raw_coupling_orders : UFO_syntax.t;
    raw_vertices : UFO_syntax.t;
    raw_lorentz : UFO_syntax.t;
    raw_parameters : UFO_syntax.t;
    raw_propagators : UFO_syntax.t;
    raw_decays : unit (* UFO_syntax.t *) }

let parse_directory dir =
  let parse stem = parse_file (Filename.concat dir (stem ^ ".py")) in
  { raw_particles = parse "particles";
    raw_couplings = parse "couplings";
    raw_coupling_orders = parse "coupling_orders";
    raw_vertices = parse "vertices";
    raw_lorentz = parse "lorentz";
    raw_parameters = parse "parameters";
    raw_propagators = parse "propagators";
    raw_decays = () (* parse "decays" *) }

let dump_file pfx f =
  List.iter
    (fun s -> print_endline (pfx ^ ": " ^ s))
    (UFO_syntax.to_strings f)

type particle = unit
type particles = particle list
type coupling = unit
type couplings = coupling list
type coupling_order = unit
type coupling_orders = coupling_order list
type vertex = unit
type vertices = vertex list
type lorentz1 = unit
type lorentz = lorentz1 list
type parameter = unit
type parameters = parameter list
type propagator = unit
type propagators = propagator list
type decay = unit
type decays = decay list

type t =
  { particles : particles;
    couplings : couplings;
    coupling_orders : coupling_orders;
    vertices : vertices;
    lorentz : lorentz;
    parameters : parameters;
    propagators : propagators;
    decays : decays }

let pass2_particles _ = []
let pass2_couplings _ = []
let pass2_coupling_orders _ = []
let pass2_vertices _ = []
let pass2_lorentz _ = []
let pass2_parameters _ = []
let pass2_propagators _ = []
let pass2_decays _ = []

let pass2 u =
  { particles = pass2_particles u.particles;
    couplings = pass2_couplings u.couplings;
    coupling_orders = pass2_coupling_orders u.coupling_orders;
    vertices = pass2_vertices u.vertices;
    lorentz = pass2_lorentz u.lorentz;
    parameters = pass2_parameters u.parameters;
    propagators = pass2_propagators u.propagators;
    decays = pass2_decays u.decays }

    
module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

