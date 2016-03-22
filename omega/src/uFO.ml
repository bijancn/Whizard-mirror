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
  | Q_Integer of int
  | Q_Fraction of int * int

let charge_to_string = function
  | Q_Integer i -> Printf.sprintf "%d" i
  | Q_Fraction (n, d) -> Printf.sprintf "%d/%d" n d

module S = UFO_syntax

let find_attrib name attribs =
  (List.find (fun a -> name = a.S.a_name) attribs).S.a_value

let integer_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer i -> i
  | _ -> invalid_arg name

let charge_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer i -> Q_Integer i
  | S.Fraction (n, d) -> Q_Fraction (n, d)
  | _ -> invalid_arg name

let string_attrib name attribs =
  match find_attrib name attribs with
  | S.String s -> s
  | _ -> invalid_arg name

type value =
  | Integer of int
  | Fraction of int * int
  | Float of float
  | String of string
  | Name of string list

let value_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer i -> Integer i
  | S.Fraction (n, d) -> Fraction (n, d)
  | S.Float x -> Float x
  | S.String s -> String s
  | S.Name n -> Name n
  | _ -> invalid_arg name

let list_attrib name attribs =
  match find_attrib name attribs with
  | S.List l -> l
  | _ -> invalid_arg name

let string_list_attrib name attribs =
  List.map
    (function
    | S.String s -> s
    | _ ->  invalid_arg name)
    (list_attrib name attribs)

let name_list_attrib name attribs =
  List.map
    (function
    | S.Name n -> n
    | _ ->  invalid_arg name)
    (list_attrib name attribs)

let integer_list_attrib name attribs =
  List.map
    (function
    | S.Integer n -> n
    | _ ->  invalid_arg name)
    (list_attrib name attribs)

let name_attrib name attribs =
  match find_attrib name attribs with
  | S.Name n -> String.concat "." (List.rev n)
  | _ -> invalid_arg name

let dictionary_attrib name attribs =
  match find_attrib name attribs with
  | S.Dictionary d -> d
  | _ -> invalid_arg name

let order_attrib name attribs =
  List.map
    (function S.Order (s, i) -> (s, i) | _ -> invalid_arg name)
    (dictionary_attrib name attribs)

let coupling_list_attrib name attribs =
  List.map
    (function
    | S.Coupling (i, j, n) -> (i, j, n)
    | _ ->  invalid_arg name)
    (dictionary_attrib name attribs)

module Particle =
  struct
    
    type t =
      { symbol : string;
	pdg_code : int;
	name : string;
	antiname : string;
	spin : int;
	color : int;
	mass : string;
	width : string;
	texname : string;
	antitexname : string;
	charge : charge;
	ghost_number : int;
	lepton_number : int;
	y : int }

    let to_string p =
      Printf.sprintf
	"particle: %s => [ pdg = %d, name = '%s'/'%s', \
                           spin = %d, color = %d, \
                           mass = %s, width = %s, \
                           Q = %s, G = %d, L = %d, Y = %d, \
                           TeX = '%s'/'%s' ]"
	p.symbol p.pdg_code p.name p.antiname
	p.spin p.color p.mass p.width
	(charge_to_string p.charge)
	p.ghost_number p.lepton_number p.y
	p.texname p.antitexname

    let find_particle symbol particles =
      List.find (fun p -> symbol = p.symbol) particles

    let conjugate_charge = function
      | Q_Integer i -> Q_Integer (-i)
      | Q_Fraction (n, d) -> Q_Fraction (-n, d)

    let conjugate symbol p =
      { symbol = symbol;
	pdg_code = - p.pdg_code;
	name = p.antiname;
	antiname = p.name;
	spin = p.spin;
	color = - p.color;
	mass = p.mass;
	width = p.width;
	texname = p.antitexname;
	antitexname = p.texname;
	charge = conjugate_charge p.charge;
	ghost_number = p.ghost_number;
	lepton_number = p.lepton_number;
	y = p.y }

    let pass2_particle acc d =
      match d.S.kind, d.S.attribs with
      | [ "Particle" ], attribs ->
	 { symbol = d.S.name;
	   pdg_code = integer_attrib "pdg_code" attribs;
	   name = string_attrib "name" attribs;
	   antiname = string_attrib "antiname" attribs;
	   spin = integer_attrib "spin" attribs;
	   color = integer_attrib "color" attribs;
	   mass = name_attrib "mass" attribs;
	   width = name_attrib "width" attribs;
	   texname = string_attrib "texname" attribs;
	   antitexname = string_attrib "antitexname" attribs;
	   charge = charge_attrib "charge" attribs;
	   ghost_number = integer_attrib "GhostNumber" attribs;
	   lepton_number = integer_attrib "LeptonNumber" attribs;
	   y = integer_attrib "Y" attribs } :: acc
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

    let pass2 particles =
      List.fold_left pass2_particle [] particles

  end
    
type coupling =
  { c_symbol : string;
    c_name : string;
    c_value : string;
    c_order : (string * int) list }

let order_to_string orders =
  String.concat ", "
    (List.map (fun (s, i) -> Printf.sprintf "'%s':%d" s i) orders)

let coupling_to_string c =
  Printf.sprintf
    "coupling: %s => [ name = '%s', value = '%s', order = [ %s ] ]"
    c.c_symbol c.c_name c.c_value
    (order_to_string c.c_order)

type couplings = coupling list

let pass2_coupling d =
  match d.S.kind, d.S.attribs with
  | [ "Coupling" ], attribs ->
     { c_symbol = d.S.name;
       c_name = string_attrib "name" attribs;
       c_value = string_attrib "value" attribs;
       c_order = order_attrib "order" attribs }
  | _ -> invalid_arg ("pass2_coupling:" ^ String.concat "." (List.rev d.S.kind))

let pass2_couplings couplings =
  List.map pass2_coupling couplings

type coupling_order =
  { o_symbol : string;
    o_name : string;
    o_expansion_order : int;
    o_hierarchy : int }

type coupling_orders = coupling_order list

let pass2_coupling_order d =
  match d.S.kind, d.S.attribs with
  | [ "CouplingOrder" ], attribs ->
     { o_symbol = d.S.name;
       o_name = string_attrib "name" attribs;
       o_expansion_order = integer_attrib "expansion_order" attribs;
       o_hierarchy = integer_attrib "hierarchy" attribs }
  | _ -> invalid_arg ("pass2_coupling_order:" ^
			 String.concat "." (List.rev d.S.kind))

let pass2_coupling_orders coupling_orders =
  List.map pass2_coupling_order coupling_orders

type vertex =
  { v_symbol : string;
    v_name : string;
    v_particles : string list list;
    v_color : string list;
    v_lorentz : string list list;
    v_couplings : (int * int * string list) list }

type vertices = vertex list

let pass2_vertex d =
  match d.S.kind, d.S.attribs with
  | [ "Vertex" ], attribs ->
     { v_symbol = d.S.name;
       v_name = string_attrib "name" attribs;
       v_particles = name_list_attrib "particles" attribs;
       v_color = string_list_attrib "color" attribs;
       v_lorentz = name_list_attrib "lorentz" attribs;
       v_couplings = coupling_list_attrib "couplings" attribs }
  | _ -> invalid_arg ("pass2_vertex:" ^
			 String.concat "." (List.rev d.S.kind))

let pass2_vertices vertices =
  List.map pass2_vertex vertices

type lorentz1 =
  { l_symbol : string;
    l_name : string;
    l_spins : int list;
    l_structure : string }

type lorentz = lorentz1 list

let pass2_lorentz1 d =
  match d.S.kind, d.S.attribs with
  | [ "Lorentz" ], attribs ->
     { l_symbol = d.S.name;
       l_name = string_attrib "name" attribs;
       l_spins = integer_list_attrib "spins" attribs;
       l_structure = string_attrib "structure" attribs }
  | _ -> invalid_arg ("pass2_lorentz:" ^
			 String.concat "." (List.rev d.S.kind))

let pass2_lorentz lorentz =
  List.map pass2_lorentz1 lorentz

type parameter =
  { pa_symbol : string;
    pa_name : string;
    pa_nature : string;
    pa_type : string;
    pa_value : value;
    pa_texname : string;
    pa_lhablock : string option;
    pa_lhacode : int list option }

type parameters = parameter list

let pass2_parameter d =
  match d.S.kind, d.S.attribs with
  | [ "Parameter" ], attribs ->
     { pa_symbol = d.S.name;
       pa_name = string_attrib "name" attribs;
       pa_nature = string_attrib "nature" attribs;
       pa_type = string_attrib "type" attribs;
       pa_value = value_attrib "value" attribs;
       pa_texname = string_attrib "texname" attribs;
       pa_lhablock =
	 (try Some (string_attrib "lhablock" attribs) with
	   Not_found -> None);
       pa_lhacode =
	 (try Some (integer_list_attrib "lhacode" attribs) with
	   Not_found -> None) }
  | _ -> invalid_arg ("pass2_parameter:" ^
			 String.concat "." (List.rev d.S.kind))
    
let pass2_parameters parameters =
  List.map pass2_parameter parameters

type propagator =
  { pr_symbol : string;
    pr_name : string;
    pr_numerator : string;
    pr_denominator : string }

type propagators = propagator list

let pass2_propagator (map, acc) d =
  match d.S.kind, d.S.attribs with
  | [ "Propagator" ], attribs ->
     let denominator =
       begin match find_attrib "denominator" attribs with
       | S.String s -> s
       | S.Name [n] -> List.assoc n map
       | _ -> invalid_arg "denominator..."
       end in
     (map,
      { pr_symbol = d.S.name;
	pr_name = string_attrib "name" attribs;
	pr_numerator = string_attrib "numerator" attribs;
	pr_denominator = denominator } :: acc)
  | [ "$"; s ], [] ->
     ((d.S.name, s) :: map, acc)
  | _ -> invalid_arg ("pass2_propagator:" ^
			 String.concat "." (List.rev d.S.kind))
       
let pass2_propagators propagators =
  let _, propagators' =
    List.fold_left pass2_propagator ([], []) propagators in
  propagators'

type decay = unit
type decays = decay list

let pass2_decays _ = []

type t =
  { particles : Particle.t list;
    couplings : couplings;
    coupling_orders : coupling_orders;
    vertices : vertices;
    lorentz : lorentz;
    parameters : parameters;
    propagators : propagators;
    decays : decays }

let pass2 u =
  { particles = Particle.pass2 u.raw_particles;
    couplings = pass2_couplings u.raw_couplings;
    coupling_orders = pass2_coupling_orders u.raw_coupling_orders;
    vertices = pass2_vertices u.raw_vertices;
    lorentz = pass2_lorentz u.raw_lorentz;
    parameters = pass2_parameters u.raw_parameters;
    propagators = pass2_propagators u.raw_propagators;
    decays = pass2_decays u.raw_decays }

let parse_directory dir =
  let result = parse_directory dir in
  let result' = pass2 result in
  List.iter
    (fun p -> print_endline (Particle.to_string p))
    result'.particles;
  List.iter
    (fun p -> print_endline (coupling_to_string p))
    result'.couplings;
  result

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

