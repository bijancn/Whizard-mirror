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

module type Files =
  sig
    
    type t =
      { particles : UFO_syntax.t;
	couplings : UFO_syntax.t;
	coupling_orders : UFO_syntax.t;
	vertices : UFO_syntax.t;
	lorentz : UFO_syntax.t;
	parameters : UFO_syntax.t;
	propagators : UFO_syntax.t;
	decays : UFO_syntax.t }

    val parse_directory : string -> t

  end

module Files : Files =
  struct
    
    type t =
      { particles : UFO_syntax.t;
	couplings : UFO_syntax.t;
	coupling_orders : UFO_syntax.t;
	vertices : UFO_syntax.t;
	lorentz : UFO_syntax.t;
	parameters : UFO_syntax.t;
	propagators : UFO_syntax.t;
	decays : UFO_syntax.t }

    let parse_directory dir =
      let parse stem = parse_file (Filename.concat dir (stem ^ ".py")) in
      { particles = parse "particles";
	couplings = parse "couplings";
	coupling_orders = parse "coupling_orders";
	vertices = parse "vertices";
	lorentz = parse "lorentz";
	parameters = parse "parameters";
	propagators = parse "propagators";
	decays = [] (* parse "decays" *) }

  end

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

let name_attrib name attribs =
  match find_attrib name attribs with
  | S.Name n -> String.concat "." (List.rev n)
  | _ -> invalid_arg name

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

let string_list_attrib name attribs =
  match find_attrib name attribs with
  | S.String_List l -> l
  | _ -> invalid_arg name

let name_list_attrib name attribs =
  match find_attrib name attribs with
  | S.Name_List l -> l
  | _ -> invalid_arg name

let integer_list_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer_List l -> l
  | _ -> invalid_arg name

let order_dictionary_attrib name attribs =
  match find_attrib name attribs with
  | S.Order_Dictionary d -> d
  | _ -> invalid_arg name

let coupling_dictionary_attrib name attribs =
  match find_attrib name attribs with
  | S.Coupling_Dictionary d -> d
  | _ -> invalid_arg name

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

    let pass2' acc d =
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
      List.fold_left pass2' [] particles

  end

module Coupling =
  struct
    
    type t =
      { symbol : string;
	name : string;
	value : string;
	order : (string * int) list }

    let order_to_string orders =
      String.concat ", "
	(List.map (fun (s, i) -> Printf.sprintf "'%s':%d" s i) orders)

    let to_string c =
      Printf.sprintf
	"coupling: %s => [ name = '%s', value = '%s', order = [ %s ] ]"
	c.symbol c.name c.value (order_to_string c.order)

    let pass2' d =
      match d.S.kind, d.S.attribs with
      | [ "Coupling" ], attribs ->
	 { symbol = d.S.name;
	   name = string_attrib "name" attribs;
	   value = string_attrib "value" attribs;
	   order = order_dictionary_attrib "order" attribs }
      | _ -> invalid_arg ("pass2_coupling:" ^ String.concat "." (List.rev d.S.kind))

    let pass2 couplings =
      List.map pass2' couplings

  end

module Coupling_Order =
  struct

    type t =
      { symbol : string;
	name : string;
	expansion_order : int;
	hierarchy : int }

    let to_string c =
      Printf.sprintf
	"coupling_order: %s => [ name = '%s', \
                                 expansion_order = '%d', \
                                 hierarchy = %d ]"
	c.symbol c.name c.expansion_order c.hierarchy

    let pass2' d =
      match d.S.kind, d.S.attribs with
      | [ "CouplingOrder" ], attribs ->
	 { symbol = d.S.name;
	   name = string_attrib "name" attribs;
	   expansion_order = integer_attrib "expansion_order" attribs;
	   hierarchy = integer_attrib "hierarchy" attribs }
      | _ -> invalid_arg ("pass2_coupling_order:" ^
			     String.concat "." (List.rev d.S.kind))

    let pass2 coupling_orders =
      List.map pass2' coupling_orders
  end

module Vertex =
  struct
    
    type t =
      { symbol : string;
	name : string;
	particles : string list list;
	color : string list;
	lorentz : string list list;
	couplings : (int * int * string list) list }

    let to_string c =
      Printf.sprintf
	"vertex: %s => [ name = '%s', particles = [ %s ], \
                         color = [ %s ], lorentz = [ %s ], \
                         couplings = [ %s ] ]"
	c.symbol c.name
	(String.concat ", "
	   (* We can strip the leading "P" *)
	   (List.map (fun n -> String.concat "." (List.rev n)) c.particles))
	(String.concat ", " c.color)
	(String.concat ", "
	   (* We can strip the leading "L" *)
	   (List.map (fun n -> String.concat "." (List.rev n)) c.lorentz))
	(String.concat ", "
	   (* We can strip the leading "C" *)
	   (List.map
	      (fun (i, j, n) ->
		Printf.sprintf "(%d,%d):" i j ^
		  String.concat "." (List.rev n))
	      c.couplings))

    let pass2' d =
      match d.S.kind, d.S.attribs with
      | [ "Vertex" ], attribs ->
	 { symbol = d.S.name;
	   name = string_attrib "name" attribs;
	   particles = name_list_attrib "particles" attribs;
	   color = string_list_attrib "color" attribs;
	   lorentz = name_list_attrib "lorentz" attribs;
	   couplings = coupling_dictionary_attrib "couplings" attribs }
      | _ -> invalid_arg ("pass2_vertex:" ^
			     String.concat "." (List.rev d.S.kind))

    let pass2 vertices =
      List.map pass2' vertices

  end

module Lorentz =
  struct
    
    type t =
      { symbol : string;
	name : string;
	spins : int list;
	structure : string }

    let pass2' d =
      match d.S.kind, d.S.attribs with
      | [ "Lorentz" ], attribs ->
	 { symbol = d.S.name;
	   name = string_attrib "name" attribs;
	   spins = integer_list_attrib "spins" attribs;
	   structure = string_attrib "structure" attribs }
      | _ -> invalid_arg ("pass2_lorentz:" ^
			     String.concat "." (List.rev d.S.kind))

    let pass2 lorentz =
      List.map pass2' lorentz

  end

module Parameter =
  struct
    
    type t =
      { symbol : string;
	name : string;
	nature : string;
	ptype : string;
	value : value;
	texname : string;
	lhablock : string option;
	lhacode : int list option }

    let pass2' d =
      match d.S.kind, d.S.attribs with
      | [ "Parameter" ], attribs ->
	 { symbol = d.S.name;
	   name = string_attrib "name" attribs;
	   nature = string_attrib "nature" attribs;
	   ptype = string_attrib "type" attribs;
	   value = value_attrib "value" attribs;
	   texname = string_attrib "texname" attribs;
	   lhablock =
	     (try Some (string_attrib "lhablock" attribs) with
	       Not_found -> None);
	   lhacode =
	     (try Some (integer_list_attrib "lhacode" attribs) with
	       Not_found -> None) }
      | _ -> invalid_arg ("pass2_parameter:" ^
			     String.concat "." (List.rev d.S.kind))
    
    let pass2 parameters =
      List.map pass2' parameters

  end

module Propagator =
  struct

    type t =
      { symbol : string;
	name : string;
	numerator : string;
	denominator : string }

    (* The parser will turn [foo = "bar"] into [foo = "bar"."$"],
       which will be interpreted as a macro definition
       for [foo] expanding to ["bar"].   The dollar is used to
       distinguish it from an empty attribute list.  This
       could also be implemented with a union. *)

    let pass2' (map, acc) d =
      match d.S.kind, d.S.attribs with
      | [ "Propagator" ], attribs ->
	 let denominator =
	   begin match find_attrib "denominator" attribs with
	   | S.String s -> s
	   | S.Name [n] -> List.assoc n map
	   | _ -> invalid_arg "denominator..."
	   end in
	 (map,
	  { symbol = d.S.name;
	    name = string_attrib "name" attribs;
	    numerator = string_attrib "numerator" attribs;
	    denominator = denominator } :: acc)
      | [ "$"; s ], [] ->
	 ((d.S.name, s) :: map, acc)
      | _ -> invalid_arg ("pass2_propagator:" ^
			     String.concat "." (List.rev d.S.kind))
       
    let pass2 propagators =
      let _, propagators' =
	List.fold_left pass2' ([], []) propagators in
      propagators'

  end

module Decay =
  struct

    type t = unit

    let pass2' _ = ()

    let pass2 decays =
      List.map pass2' decays

  end

type t =
  { particles : Particle.t list;
    couplings : Coupling.t list;
    coupling_orders : Coupling_Order.t list;
    vertices : Vertex.t list;
    lorentz : Lorentz.t list;
    parameters : Parameter.t list;
    propagators : Propagator.t list;
    decays : Decay.t list }

let pass2 u =
  { particles = Particle.pass2 u.Files.particles;
    couplings = Coupling.pass2 u.Files.couplings;
    coupling_orders = Coupling_Order.pass2 u.Files.coupling_orders;
    vertices = Vertex.pass2 u.Files.vertices;
    lorentz = Lorentz.pass2 u.Files.lorentz;
    parameters = Parameter.pass2 u.Files.parameters;
    propagators = Propagator.pass2 u.Files.propagators;
    decays = Decay.pass2 u.Files.decays }

let parse_directory dir =
  let result = pass2 (Files.parse_directory dir) in
  List.iter
    (fun p -> print_endline (Particle.to_string p))
    result.particles;
  List.iter
    (fun p -> print_endline (Coupling.to_string p))
    result.couplings;
  List.iter
    (fun p -> print_endline (Coupling_Order.to_string p))
    result.coupling_orders;
  List.iter
    (fun p -> print_endline (Vertex.to_string p))
    result.vertices;
  result

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

