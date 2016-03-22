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

type charge =
  | Integer of int
  | Fraction of int * int

type particle =
  { p_symbol : string;
    p_pdg_code : int;
    p_name : string;
    p_antiname : string;
    p_spin : int;
    p_color : int;
    p_mass : string;
    p_width : string;
    p_texname : string;
    p_antitexname : string;
    p_charge : charge;
    p_GhostNumber : int;
    p_LeptonNumber : int;
    p_Y : int }

type particles = particle list

module S = UFO_syntax

let find_attrib name attribs =
  (List.find (fun a -> name = a.S.a_name) attribs).S.a_value

let integer_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer i -> i
  | _ -> invalid_arg name

let fraction_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer i -> Integer i
  | S.Fraction (n, d) -> Fraction (n, d)
  | _ -> invalid_arg name

let string_attrib name attribs =
  match find_attrib name attribs with
  | S.String s -> s
  | _ -> invalid_arg name

let name_attrib name attribs =
  match find_attrib name attribs with
  | S.Name n -> String.concat "." (List.rev n)
  | _ -> invalid_arg name

let find_particle symbol particles =
  List.find (fun p -> symbol = p.p_symbol) particles

let conjugate_charge = function
  | Integer i -> Integer (-i)
  | Fraction (n, d) -> Fraction (-n, d)

let conjugate symbol p =
  { p_symbol = symbol;
    p_pdg_code = - p.p_pdg_code;
    p_name = p.p_antiname;
    p_antiname = p.p_name;
    p_spin = p.p_spin;
    p_color = - p.p_color;
    p_mass = p.p_mass;
    p_width = p.p_width;
    p_texname = p.p_antitexname;
    p_antitexname = p.p_texname;
    p_charge = conjugate_charge p.p_charge;
    p_GhostNumber = p.p_GhostNumber;
    p_LeptonNumber = p.p_LeptonNumber;
    p_Y = p.p_Y }

let pass2_particle acc d =
  match d.S.kind, d.S.attribs with
  | [ ( "Particle" | "particle" ) ], attribs ->
     { p_symbol = d.S.name;
       p_pdg_code = integer_attrib "pdg_code" attribs;
       p_name = string_attrib "name" attribs;
       p_antiname = string_attrib "antiname" attribs;
       p_spin = integer_attrib "spin" attribs;
       p_color = integer_attrib "color" attribs;
       p_mass = name_attrib "mass" attribs;
       p_width = name_attrib "width" attribs;
       p_texname = string_attrib "texname" attribs;
       p_antitexname = string_attrib "antitexname" attribs;
       p_charge = fraction_attrib "charge" attribs;
       p_GhostNumber = integer_attrib "GhostNumber" attribs;
       p_LeptonNumber = integer_attrib "LeptonNumber" attribs;
       p_Y = integer_attrib "Y" attribs } :: acc
  | [ "anti"; p ], [] ->
     begin
       try
	 let anti = find_particle p acc in
	 conjugate d.S.name anti :: acc
       with
       | Not_found ->
	  failwith ("UFO.pass2_particle: " ^ p ^ ".anti() not yet defined!")
     end
  | _ -> invalid_arg ("pass2_particle:" ^ String.concat "." (List.rev d.S.kind))

let pass2_particles particles =
  List.fold_left pass2_particle [] particles

type coupling = unit
type couplings = coupling list

let pass2_couplings _ = []

type coupling_order = unit
type coupling_orders = coupling_order list

let pass2_coupling_orders _ = []

type vertex = unit
type vertices = vertex list
let pass2_vertices _ = []

type lorentz1 = unit
type lorentz = lorentz1 list

let pass2_lorentz _ = []

type parameter = unit
type parameters = parameter list

let pass2_parameters _ = []

type propagator = unit
type propagators = propagator list

let pass2_propagators _ = []

type decay = unit
type decays = decay list

let pass2_decays _ = []

type t =
  { particles : particles;
    couplings : couplings;
    coupling_orders : coupling_orders;
    vertices : vertices;
    lorentz : lorentz;
    parameters : parameters;
    propagators : propagators;
    decays : decays }

let pass2 u =
  { particles = pass2_particles u.raw_particles;
    couplings = pass2_couplings u.raw_couplings;
    coupling_orders = pass2_coupling_orders u.raw_coupling_orders;
    vertices = pass2_vertices u.raw_vertices;
    lorentz = pass2_lorentz u.raw_lorentz;
    parameters = pass2_parameters u.raw_parameters;
    propagators = pass2_propagators u.raw_propagators;
    decays = pass2_decays u.raw_decays }

let parse_directory dir =
  let result = parse_directory dir in
  ignore (pass2 result);
  result

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

