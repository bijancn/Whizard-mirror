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
	decays = parse "decays" }

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

let name_to_string ?strip name =
  let stripped =
    begin match strip, List.rev name with
    | Some pfx, head :: tail ->
       if pfx = head then
	 tail
       else
	 failwith ("UFO.name_to_string: expected prefix '" ^ pfx ^
		      "', got '" ^ head ^ "'")
    | _, name -> name
    end in
  String.concat "." stripped

let name_attrib ~strip name attribs =
  match find_attrib name attribs with
  | S.Name n -> name_to_string ~strip n
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

let value_to_string = function
  | Integer i -> Printf.sprintf "%d" i
  | Fraction (n, d) -> Printf.sprintf "%d/%d" n d
  | Float x -> Printf.sprintf "%f" x
  | String s -> Printf.sprintf "'%s'" s
  | Name n -> name_to_string n

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

let name_list_attrib ~strip name attribs =
  match find_attrib name attribs with
  | S.Name_List l -> List.map (name_to_string ~strip) l
  | _ -> invalid_arg name

let integer_list_attrib name attribs =
  match find_attrib name attribs with
  | S.Integer_List l -> l
  | _ -> invalid_arg name

let order_dictionary_attrib name attribs =
  match find_attrib name attribs with
  | S.Order_Dictionary d -> d
  | _ -> invalid_arg name

let coupling_dictionary_attrib ~strip name attribs =
  match find_attrib name attribs with
  | S.Coupling_Dictionary d ->
     List.map (fun (i, j, c) -> (i, j, name_to_string ~strip c)) d
  | _ -> invalid_arg name

let decay_dictionary_attrib name attribs =
  match find_attrib name attribs with
  | S.Decay_Dictionary d ->
     List.map (fun (p, w) -> (List.map List.hd p, w)) d
  | _ -> invalid_arg name

module SMap = Map.Make (struct type t = string let compare = compare end)

module Particle =
  struct
    
    type t =
      { pdg_code : int;
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
	y : int;
	goldstone : bool;    (* NOT HANDLED YET! *)
	propagating : bool;  (* NOT HANDLED YET! *)
	line : string option (* NOT HANDLED YET! *) }

    let to_string symbol p =
      Printf.sprintf
	"particle: %s => [ pdg = %d, name = '%s'/'%s', \
                           spin = %d, color = %d, \
                           mass = %s, width = %s, \
                           Q = %s, G = %d, L = %d, Y = %d, \
                           TeX = '%s'/'%s' ]"
	symbol p.pdg_code p.name p.antiname
	p.spin p.color p.mass p.width
	(charge_to_string p.charge)
	p.ghost_number p.lepton_number p.y
	p.texname p.antitexname

    let conjugate_charge = function
      | Q_Integer i -> Q_Integer (-i)
      | Q_Fraction (n, d) -> Q_Fraction (-n, d)

    let conjugate p =
      { pdg_code = - p.pdg_code;
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
	y = p.y;
	goldstone = p.goldstone;
	propagating = p.propagating;
	line = p.line }

    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Particle" ], attribs ->
	 SMap.add symbol
	   { pdg_code = integer_attrib "pdg_code" attribs;
	     name = string_attrib "name" attribs;
	     antiname = string_attrib "antiname" attribs;
	     spin = integer_attrib "spin" attribs;
	     color = integer_attrib "color" attribs;
	     mass = name_attrib ~strip:"Param" "mass" attribs;
	     width = name_attrib ~strip:"Param" "width" attribs;
	     texname = string_attrib "texname" attribs;
	     antitexname = string_attrib "antitexname" attribs;
	     charge = charge_attrib "charge" attribs;
	     ghost_number = integer_attrib "GhostNumber" attribs;
	     lepton_number = integer_attrib "LeptonNumber" attribs;
	     y = integer_attrib "Y" attribs;
	     goldstone = false;
	     propagating = true;
	     line = None } map
      | [ "anti"; p ], [] ->
	 begin
	   try
	     SMap.add symbol (conjugate (SMap.find p map)) map
	   with
	   | Not_found ->
	      failwith ("UFO.pass2_particle: " ^ p ^ ".anti() not yet defined!")
	 end
      | _ -> invalid_arg ("pass2_particle:" ^ name_to_string d.S.kind)

    let pass2 particles =
      List.fold_left pass2' SMap.empty particles

  end

module Coupling =
  struct
    
    type t =
      { name : string;
	value : string;
	order : (string * int) list }

    let order_to_string orders =
      String.concat ", "
	(List.map (fun (s, i) -> Printf.sprintf "'%s':%d" s i) orders)

    let to_string symbol c =
      Printf.sprintf
	"coupling: %s => [ name = '%s', value = '%s', order = [ %s ] ]"
	symbol c.name c.value (order_to_string c.order)

    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Coupling" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     value = string_attrib "value" attribs;
	     order = order_dictionary_attrib "order" attribs } map
      | _ -> invalid_arg ("pass2_coupling:" ^ name_to_string d.S.kind)

    let pass2 couplings =
      List.fold_left pass2' SMap.empty couplings

  end

module Coupling_Order =
  struct

    type t =
      { name : string;
	expansion_order : int;
	hierarchy : int }

    let to_string symbol c =
      Printf.sprintf
	"coupling_order: %s => [ name = '%s', \
                                 expansion_order = '%d', \
                                 hierarchy = %d ]"
	symbol c.name c.expansion_order c.hierarchy

    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "CouplingOrder" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     expansion_order = integer_attrib "expansion_order" attribs;
	     hierarchy = integer_attrib "hierarchy" attribs } map
      | _ -> invalid_arg ("pass2_coupling_order:" ^ name_to_string d.S.kind)

    let pass2 coupling_orders =
      List.fold_left pass2' SMap.empty coupling_orders
  end

module Vertex =
  struct
    
    type t =
      { name : string;
	particles : string list;
	color : UFOx.Color.t list;
	lorentz : string list;
	couplings : (int * int * string) list }

    let to_string symbol c =
      Printf.sprintf
	"vertex: %s => [ name = '%s', particles = [ %s ], \
                         color = [ %s ], lorentz = [ %s ], \
                         couplings = [ %s ] ]"
	symbol c.name
	(String.concat ", " c.particles)
	(String.concat ", " (List.map UFOx.Color.to_string c.color))
	(String.concat ", " c.lorentz)
	(String.concat ", "
	   (List.map
	      (fun (i, j, n) -> Printf.sprintf "(%d,%d): %s" i j n)
	      c.couplings))

    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Vertex" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     particles = name_list_attrib ~strip:"P" "particles" attribs;
	     color =
	       List.map
		 UFOx.Color.of_string (string_list_attrib "color" attribs);
	     lorentz = name_list_attrib ~strip:"L" "lorentz" attribs;
	     couplings =
	       coupling_dictionary_attrib ~strip:"C" "couplings" attribs } map
      | _ -> invalid_arg ("pass2_vertex:" ^ name_to_string d.S.kind)

    let pass2 vertices =
      List.fold_left pass2' SMap.empty vertices

  end

module Lorentz =
  struct
    
    type t =
      { name : string;
	spins : int list;
	structure : UFOx.Lorentz.t }

    let to_string symbol l =
      Printf.sprintf
	"lorentz: %s => [ name = '%s', spins = [ %s ], \
                          structure = %s ]"
	symbol l.name
	(String.concat ", " (List.map string_of_int l.spins))
	(UFOx.Lorentz.to_string l.structure)

    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Lorentz" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     spins = integer_list_attrib "spins" attribs;
	     structure =
	       UFOx.Lorentz.of_string (string_attrib "structure" attribs) } map
      | _ -> invalid_arg ("pass2_lorentz:" ^ name_to_string d.S.kind)

    let pass2 lorentz =
      List.fold_left pass2' SMap.empty lorentz

  end

module Parameter =
  struct

    type nature = Internal | External
	
    let nature_to_string = function
      | Internal -> "internal"
      | External -> "external"

    let nature_of_string = function
      | "internal" -> Internal
      | "external" -> External
      | s -> invalid_arg ("Parameter.nature_of_string: " ^ s)
	 
    type ptype = Real | Complex

    let ptype_to_string = function
      | Real -> "real"
      | Complex -> "complex"

    let ptype_of_string = function
      | "real" -> Real
      | "complex" -> Complex
      | s -> invalid_arg ("Parameter.ptype_of_string: " ^ s)

    type t =
      { name : string;
	nature : nature;
	ptype : ptype;
	value : value;
	texname : string;
	lhablock : string option;
	lhacode : int list option }

    let to_string symbol p =
      Printf.sprintf
	"parameter: %s => [ name = '%s', nature = %s, type = %s, \
                            value = %s, texname = '%s', \
                            lhablock = %s, lhacode = [ %s ] ]"
	symbol p.name
	(nature_to_string p.nature)
	(ptype_to_string p.ptype)
	(value_to_string p.value) p.texname
	(match p.lhablock with None -> "???" | Some s -> s)
	(match p.lhacode with
	| None -> ""
	| Some c -> String.concat ", " (List.map string_of_int c))
      
    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Parameter" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     nature = nature_of_string (string_attrib "nature" attribs);
	     ptype = ptype_of_string (string_attrib "type" attribs);
	     value = value_attrib "value" attribs;
	     texname = string_attrib "texname" attribs;
	     lhablock =
	       (try Some (string_attrib "lhablock" attribs) with
		 Not_found -> None);
	     lhacode =
	       (try Some (integer_list_attrib "lhacode" attribs) with
		 Not_found -> None) } map
      | _ -> invalid_arg ("pass2_parameter:" ^ name_to_string d.S.kind)
    
    let pass2 parameters =
      List.fold_left pass2' SMap.empty parameters

  end

module Propagator =
  struct

    type t =
      { name : string;
	numerator : string;
	denominator : string }

    let to_string symbol p =
      Printf.sprintf
	"propagator: %s => [ name = '%s', numerator = '%s', \
                             denominator = '%s' ]"
	symbol p.name p.numerator p.denominator
      
    (* The parser will turn [foo = "bar"] into [foo = "bar"."$"],
       which will be interpreted as a macro definition
       for [foo] expanding to ["bar"].   The dollar is used to
       distinguish it from an empty attribute list.  This
       could also be implemented with a union type for the
       declarations.  *)

    let pass2' (macros, map) d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Propagator" ], attribs ->
	 let denominator =
	   begin match find_attrib "denominator" attribs with
	   | S.String s -> s
	   | S.Name [n] -> SMap.find n macros
	   | _ -> invalid_arg "denominator..."
	   end in
	 (macros,
	  SMap.add symbol
	    { name = string_attrib "name" attribs;
	      numerator = string_attrib "numerator" attribs;
	      denominator = denominator } map)
      | [ "$"; s ], [] ->
	 (SMap.add symbol s macros, map)
      | _ -> invalid_arg ("pass2_propagator:" ^ name_to_string d.S.kind)
       
    let pass2 propagators =
      let _, propagators' =
	List.fold_left pass2' (SMap.empty, SMap.empty) propagators in
      propagators'

  end

module Decay =
  struct

    type t =
      { name : string;
	particle : string;
	widths : (string list * string) list }

    let width_to_string ws =
      String.concat ", "
	(List.map
	   (fun (ps, w) ->
	     "(" ^ String.concat ", " ps ^ ") -> '" ^ w ^ "'")
	   ws)

    let to_string symbol d =
      Printf.sprintf
	"decay: %s => [ name = '%s', particle = '%s', widths = [ %s ] ]"
	symbol d.name d.particle (width_to_string d.widths)

    let pass2' map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Decay" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     particle = name_attrib ~strip:"P" "particle" attribs;
	     widths = decay_dictionary_attrib "partial_widths" attribs } map
      | _ -> invalid_arg ("pass2_decay:" ^ name_to_string d.S.kind)

    let pass2 decays =
      List.fold_left pass2' SMap.empty decays

  end

type t =
  { particles : Particle.t SMap.t;
    couplings : Coupling.t SMap.t;
    coupling_orders : Coupling_Order.t SMap.t;
    vertices : Vertex.t SMap.t;
    lorentz : Lorentz.t SMap.t;
    parameters : Parameter.t SMap.t;
    propagators : Propagator.t SMap.t;
    decays : Decay.t SMap.t}

let pass2 u =
  { particles = Particle.pass2 u.Files.particles;
    couplings = Coupling.pass2 u.Files.couplings;
    coupling_orders = Coupling_Order.pass2 u.Files.coupling_orders;
    vertices = Vertex.pass2 u.Files.vertices;
    lorentz = Lorentz.pass2 u.Files.lorentz;
    parameters = Parameter.pass2 u.Files.parameters;
    propagators = Propagator.pass2 u.Files.propagators;
    decays = Decay.pass2 u.Files.decays }

let (@@@) f g x y =
  f (g x y)

let parse_directory dir =
  let result = pass2 (Files.parse_directory dir) in
  SMap.iter (print_endline @@@ Particle.to_string) result.particles;
  SMap.iter
    (fun symbol c ->
      (print_endline @@@ Coupling.to_string) symbol c;
      ignore (UFOx.Expr.of_string c.Coupling.value))
    result.couplings;
  SMap.iter (print_endline @@@ Coupling_Order.to_string) result.coupling_orders;
  SMap.iter (print_endline @@@ Vertex.to_string) result.vertices;
  SMap.iter (print_endline @@@ Lorentz.to_string) result.lorentz;
  SMap.iter (print_endline @@@ Parameter.to_string) result.parameters;
  SMap.iter (print_endline @@@ Propagator.to_string) result.propagators;
  SMap.iter
    (fun symbol d ->
      print_endline (Decay.to_string symbol d);
      List.iter (fun (_, w) -> ignore (UFOx.Expr.of_string w)) d.Decay.widths)
    result.decays;
  result

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

