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

let rcs_file = RCS.parse "UFO" ["Reading UFO Files"]
    { RCS.revision = "$Revision: 0$";
      RCS.date = "$Date: 2016-03-26 00:00:00 +0100 (Sat, 26 Mar 2016) $";
      RCS.author = "$Author: ohl $";
      RCS.source
        = "$URL: svn+ssh://login.hepforge.org/hepforge/svn/whizard/trunk/omega/src/UFO.ml $" }

let (@<) f g x =
 f (g x)

let (@@<) f g x y =
  f (g x y)

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
    
    type t = private
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

let name_attrib ?strip name attribs =
  match find_attrib name attribs with
  | S.Name n -> name_to_string ?strip n
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

let boolean_attrib name attribs =
  try
    match String.lowercase (name_attrib name attribs) with
    | "true" -> true
    | "false" -> false
    | _ -> invalid_arg name
  with
  | Not_found -> false

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

let map_to_alist map =
  SMap.fold (fun key value acc -> (key, value) :: acc) map []

let keys map =
  SMap.fold (fun key _ acc -> key :: acc) map []

let values map =
  SMap.fold (fun _ value acc -> value :: acc) map []

module SKey =
  struct
    type t = string
    let hash = Hashtbl.hash
    let equal = (=)
  end
module SHash = Hashtbl.Make (SKey)

module type Particle =
  sig

    type t = private
      { pdg_code : int;
	name : string;
	antiname : string;
	spin : UFOx.Lorentz.r;
	color : UFOx.Color.r;
	mass : string;
	width : string;
	texname : string;
	antitexname : string;
	charge : charge;
	ghost_number : int;
	lepton_number : int;
	y : int;
	goldstone : bool;
	propagating : bool;  (* NOT HANDLED YET! *)
	line : string option (* NOT HANDLED YET! *) }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string
    val conjugate : t -> t
    val is_ghost : t -> bool
    val is_goldstone : t -> bool
    val is_physical : t -> bool
    val filter : (t -> bool) -> t SMap.t -> t SMap.t

  end

module Particle : Particle =
  struct
    
    type t =
      { pdg_code : int;
	name : string;
	antiname : string;
	spin : UFOx.Lorentz.r;
	color : UFOx.Color.r;
	mass : string;
	width : string;
	texname : string;
	antitexname : string;
	charge : charge;
	ghost_number : int;
	lepton_number : int;
	y : int;
	goldstone : bool;
	propagating : bool;  (* NOT HANDLED YET! *)
	line : string option (* NOT HANDLED YET! *) }

    let to_string symbol p =
      Printf.sprintf
	"particle: %s => [pdg = %d, name = '%s'/'%s', \
                          spin = %s, color = %s, \
                          mass = %s, width = %s, \
                          Q = %s, G = %d, L = %d, Y = %d, \
                          TeX = '%s'/'%s'%s]"
	symbol p.pdg_code p.name p.antiname
	(UFOx.Lorentz.rep_to_string p.spin)
	(UFOx.Color.rep_to_string p.color)
	p.mass p.width
	(charge_to_string p.charge)
	p.ghost_number p.lepton_number p.y
	p.texname p.antitexname
	(if p.goldstone then ", GB" else "")

    let conjugate_charge = function
      | Q_Integer i -> Q_Integer (-i)
      | Q_Fraction (n, d) -> Q_Fraction (-n, d)

    let is_neutral p =
      (p.name = p.antiname)

    (* We \emph{must not} mess with [pdg_code] and [color] if
       the particle is neutral! *)
    let conjugate p =
      if is_neutral p then
	p
      else
	{ pdg_code = - p.pdg_code;
	  name = p.antiname;
	  antiname = p.name;
	  spin = UFOx.Lorentz.rep_conjugate p.spin;
	  color = UFOx.Color.rep_conjugate p.color;
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

    let of_file1 map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Particle" ], attribs ->
	 SMap.add symbol
	   { pdg_code = integer_attrib "pdg_code" attribs;
	     name = string_attrib "name" attribs;
	     antiname = string_attrib "antiname" attribs;
	     spin = UFOx.Lorentz.rep_of_int (integer_attrib "spin" attribs);
	     color = UFOx.Color.rep_of_int (integer_attrib "color" attribs);
	     mass = name_attrib ~strip:"Param" "mass" attribs;
	     width = name_attrib ~strip:"Param" "width" attribs;
	     texname = string_attrib "texname" attribs;
	     antitexname = string_attrib "antitexname" attribs;
	     charge = charge_attrib "charge" attribs;
	     ghost_number = integer_attrib "GhostNumber" attribs;
	     lepton_number = integer_attrib "LeptonNumber" attribs;
	     y = integer_attrib "Y" attribs;
	     goldstone = boolean_attrib "goldstone" attribs;
	     propagating = true;
	     line = None } map
      | [ "anti"; p ], [] ->
	 begin
	   try
	     SMap.add symbol (conjugate (SMap.find p map)) map
	   with
	   | Not_found ->
	      invalid_arg
		("Particle.of_file: " ^ p ^ ".anti() not yet defined!")
	 end
      | _ -> invalid_arg ("Particle.of_file: " ^ name_to_string d.S.kind)

    let of_file particles =
      List.fold_left of_file1 SMap.empty particles

    let is_ghost p =
      p.ghost_number <> 0

    let is_goldstone p =
      p.goldstone

    let is_physical p =
      not (is_ghost p || is_goldstone p)

    let filter predicate map =
      SMap.filter (fun symbol p -> predicate p) map

  end

module type UFO_Coupling =
  sig

    type t = private
      { name : string;
	value : string;
	order : (string * int) list }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

  end

module UFO_Coupling : UFO_Coupling =
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
	"coupling: %s => [name = '%s', value = '%s', order = [%s]]"
	symbol c.name c.value (order_to_string c.order)

    let of_file1 map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Coupling" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     value = string_attrib "value" attribs;
	     order = order_dictionary_attrib "order" attribs } map
      | _ -> invalid_arg ("UFO_Coupling.of_file: " ^ name_to_string d.S.kind)

    let of_file couplings =
      List.fold_left of_file1 SMap.empty couplings

  end

module type Coupling_Order =
  sig

    type t = private
      { name : string;
	expansion_order : int;
	hierarchy : int }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

  end

module Coupling_Order : Coupling_Order =
  struct

    type t =
      { name : string;
	expansion_order : int;
	hierarchy : int }

    let to_string symbol c =
      Printf.sprintf
	"coupling_order: %s => [name = '%s', \
                                expansion_order = '%d', \
                                hierarchy = %d]"
	symbol c.name c.expansion_order c.hierarchy

    let of_file1 map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "CouplingOrder" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     expansion_order = integer_attrib "expansion_order" attribs;
	     hierarchy = integer_attrib "hierarchy" attribs } map
      | _ -> invalid_arg ("Coupling_order.of_file: " ^ name_to_string d.S.kind)

    let of_file coupling_orders =
      List.fold_left of_file1 SMap.empty coupling_orders
  end

module type Vertex =
  sig

    type t = private
      { name : string;
	particles : string array;
	color : UFOx.Color.t array;
	lorentz : string array;
	couplings : string option array array }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

    val contains : Particle.t SMap.t -> (Particle.t -> bool) -> t -> bool
    val filter : (t -> bool) -> t SMap.t -> t SMap.t

  end

module Vertex : Vertex =
  struct
    
    type t =
      { name : string;
	particles : string array;
	color : UFOx.Color.t array;
	lorentz : string array;
	couplings : string option array array }

    let to_string symbol c =
      Printf.sprintf
	"vertex: %s => [name = '%s', particles = [%s], \
                        color = [%s], lorentz = [%s], \
                        couplings = [%s]]"
	symbol c.name
	(String.concat ", " (Array.to_list c.particles))
	(String.concat ", "
	   (List.map UFOx.Color.to_string (Array.to_list c.color)))
	(String.concat ", " (Array.to_list c.lorentz))
	(String.concat ", "
	   (List.map
	      (fun column ->
		"[" ^ (String.concat ", "
			 (List.map
			    (function Some s -> s | None -> "0")
			    (Array.to_list column))) ^ "]")
	      (Array.to_list c.couplings)))

    let contains particles predicate v =
      let p = v.particles in
      let rec contains' i =
	if i < 0 then
	  false
	else if predicate (SMap.find p.(i) particles) then
	  true
	else
	  contains' (pred i) in
      contains' (Array.length p - 1)
      
    let of_file1 map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Vertex" ], attribs ->
	 let color =
	   Array.of_list
	     (List.map
		UFOx.Color.of_string (string_list_attrib "color" attribs))
	 and lorentz =
	   Array.of_list (name_list_attrib ~strip:"L" "lorentz" attribs)
	 and couplings_alist =
	   coupling_dictionary_attrib ~strip:"C" "couplings" attribs in
	 let couplings =
	   Array.make_matrix (Array.length color) (Array.length lorentz) None in
	 List.iter
	   (fun (i, j, c) -> couplings.(i).(j) <- Some c)
	   couplings_alist;
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     particles =
	       Array.of_list (name_list_attrib ~strip:"P" "particles" attribs);
	     color = color;
	     lorentz = lorentz;
	     couplings = couplings } map
      | _ -> invalid_arg ("Vertex.of_file: " ^ name_to_string d.S.kind)

    let of_file vertices =
      List.fold_left of_file1 SMap.empty vertices

    let filter predicate map =
      SMap.filter (fun symbol p -> predicate p) map

  end

module type Lorentz =
  sig

    type t = private
      { name : string;
	spins : int list;
	structure : UFOx.Lorentz.t }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

  end

module Lorentz : Lorentz =
  struct
    
    type t =
      { name : string;
	spins : int list;
	structure : UFOx.Lorentz.t }

    let to_string symbol l =
      Printf.sprintf
	"lorentz: %s => [name = '%s', spins = [%s], \
                         structure = %s]"
	symbol l.name
	(String.concat ", " (List.map string_of_int l.spins))
	(UFOx.Lorentz.to_string l.structure)

    let of_file1 map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Lorentz" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     spins = integer_list_attrib "spins" attribs;
	     structure =
	       UFOx.Lorentz.of_string (string_attrib "structure" attribs) } map
      | _ -> invalid_arg ("Lorentz.of_file: " ^ name_to_string d.S.kind)

    let of_file lorentz =
      List.fold_left of_file1 SMap.empty lorentz

  end

module type Parameter =
  sig

    type nature = private Internal | External
    type ptype = private Real | Complex

    type t = private
      { name : string;
	nature : nature;
	ptype : ptype;
	value : value;
	texname : string;
	lhablock : string option;
	lhacode : int list option }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

  end

module Parameter : Parameter =
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
	"parameter: %s => [name = '%s', nature = %s, type = %s, \
                           value = %s, texname = '%s', \
                           lhablock = %s, lhacode = [%s]]"
	symbol p.name
	(nature_to_string p.nature)
	(ptype_to_string p.ptype)
	(value_to_string p.value) p.texname
	(match p.lhablock with None -> "???" | Some s -> s)
	(match p.lhacode with
	| None -> ""
	| Some c -> String.concat ", " (List.map string_of_int c))
      
    let of_file1 map d =
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
      | _ -> invalid_arg ("Parameter.of_file: " ^ name_to_string d.S.kind)
    
    let of_file parameters =
      List.fold_left of_file1 SMap.empty parameters

  end

module type Propagator =
  sig

    type t = private
      { name : string;
	numerator : string;
	denominator : string }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

  end

module Propagator : Propagator =
  struct

    type t =
      { name : string;
	numerator : string;
	denominator : string }

    let to_string symbol p =
      Printf.sprintf
	"propagator: %s => [name = '%s', numerator = '%s', \
                            denominator = '%s']"
	symbol p.name p.numerator p.denominator
      
    (* The parser will turn [foo = "bar"] into [foo = "bar"."$"],
       which will be interpreted as a macro definition
       for [foo] expanding to ["bar"].   The dollar is used to
       distinguish it from an empty attribute list.  This
       could also be implemented with a union type for the
       declarations.  *)

    let of_file1 (macros, map) d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Propagator" ], attribs ->
	 let denominator =
	   begin match find_attrib "denominator" attribs with
	   | S.String s -> s
	   | S.Name [n] -> SMap.find n macros
	   | _ -> invalid_arg "Propagator.denominator: "
	   end in
	 (macros,
	  SMap.add symbol
	    { name = string_attrib "name" attribs;
	      numerator = string_attrib "numerator" attribs;
	      denominator = denominator } map)
      | [ "$"; s ], [] ->
	 (SMap.add symbol s macros, map)
      | _ -> invalid_arg ("Propagator:of_file: " ^ name_to_string d.S.kind)
       
    let of_file propagators =
      let _, propagators' =
	List.fold_left of_file1 (SMap.empty, SMap.empty) propagators in
      propagators'

  end

module type Decay =
  sig

    type t = private
      { name : string;
	particle : string;
	widths : (string list * string) list }

    val of_file : S.t -> t SMap.t
    val to_string : string -> t -> string

  end

module Decay : Decay =
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
	"decay: %s => [name = '%s', particle = '%s', widths = [%s]]"
	symbol d.name d.particle (width_to_string d.widths)

    let of_file1 map d =
      let symbol = d.S.name in
      match d.S.kind, d.S.attribs with
      | [ "Decay" ], attribs ->
	 SMap.add symbol
	   { name = string_attrib "name" attribs;
	     particle = name_attrib ~strip:"P" "particle" attribs;
	     widths = decay_dictionary_attrib "partial_widths" attribs } map
      | _ -> invalid_arg ("Decay.of_file: " ^ name_to_string d.S.kind)

    let of_file decays =
      List.fold_left of_file1 SMap.empty decays

  end

type t =
  { particles : Particle.t SMap.t;
    couplings : UFO_Coupling.t SMap.t;
    coupling_orders : Coupling_Order.t SMap.t;
    vertices : Vertex.t SMap.t;
    lorentz : Lorentz.t SMap.t;
    parameters : Parameter.t SMap.t;
    propagators : Propagator.t SMap.t;
    decays : Decay.t SMap.t }

(* Take the elements of [list] that satisfy [predicate] and
   form a list of pairs of offsets and elements with the offsets
   starting from [offset]. *)
let alist_of_list predicate offset list =
  let _, alist =
    List.fold_left
      (fun (n, acc) x ->
	(succ n, if predicate x then (n, x) :: acc else acc))
      (offset, []) list in
  alist

let lorentz_reps_of_vertex model v =
  alist_of_list (not @< UFOx.Lorentz.rep_trivial) 1
    (List.map
       (fun p ->
	 (* Why do we need to conjugate??? *)
	 UFOx.Lorentz.rep_conjugate
	   (SMap.find p model.particles).Particle.spin)
       (Array.to_list v.Vertex.particles))

let check_lorentz_reps_of_vertex model v =
  let reps_particles = List.sort compare (lorentz_reps_of_vertex model v) in
  Array.iter
    (fun l ->
      let l = (SMap.find l model.lorentz).Lorentz.structure in
      let reps_vertex = List.sort compare (UFOx.Lorentz.classify_indices l) in
      if reps_vertex <> reps_particles then begin
	Printf.printf "%s <> %s\n"
	  (UFOx.Index.classes_to_string
	     UFOx.Lorentz.rep_to_string reps_particles)
	  (UFOx.Index.classes_to_string
	     UFOx.Lorentz.rep_to_string reps_vertex);
	invalid_arg "check_lorentz_reps_of_vertex"
      end)
    v.Vertex.lorentz
  
let color_reps_of_vertex model v =
  alist_of_list (not @< UFOx.Color.rep_trivial) 1
    (List.map
       (fun p -> (SMap.find p model.particles).Particle.color)
       (Array.to_list v.Vertex.particles))

let check_color_reps_of_vertex model v =
  let reps_particles = List.sort compare (color_reps_of_vertex model v) in
  Array.iter
    (fun c ->
      let reps_vertex = List.sort compare (UFOx.Color.classify_indices c) in
      if reps_vertex <> reps_particles then begin
	Printf.printf "%s <> %s\n"
	  (UFOx.Index.classes_to_string UFOx.Color.rep_to_string reps_particles)
	  (UFOx.Index.classes_to_string UFOx.Color.rep_to_string reps_vertex);
	invalid_arg "check_color_reps_of_vertex"
     end)
    v.Vertex.color
  
let of_file u =
  let model =
    { particles = Particle.of_file u.Files.particles;
      couplings = UFO_Coupling.of_file u.Files.couplings;
      coupling_orders = Coupling_Order.of_file u.Files.coupling_orders;
      vertices = Vertex.of_file u.Files.vertices;
      lorentz = Lorentz.of_file u.Files.lorentz;
      parameters = Parameter.of_file u.Files.parameters;
      propagators = Propagator.of_file u.Files.propagators;
      decays = Decay.of_file u.Files.decays } in
  SMap.iter
    (fun _ v ->
      check_color_reps_of_vertex model v;
      check_lorentz_reps_of_vertex model v)
    model.vertices;
  model

let parse_directory dir =
  of_file (Files.parse_directory dir)

let dump model =
  SMap.iter (print_endline @@< Particle.to_string) model.particles;
  (* SMap.iter (print_endline @@< UFO_Coupling.to_string) model.couplings; *)
  SMap.iter
    (fun symbol c ->
      (print_endline @@< UFO_Coupling.to_string) symbol c;
      print_endline
	(UFOx.Value.to_string
	   (UFOx.Value.of_expr (UFOx.Expr.of_string c.UFO_Coupling.value))))
    model.couplings;
  SMap.iter (print_endline @@< Coupling_Order.to_string) model.coupling_orders;
  (* SMap.iter (print_endline @@< Vertex.to_string) model.vertices; *)
  SMap.iter
    (fun symbol v ->
      (print_endline @@< Vertex.to_string) symbol v;
      check_color_reps_of_vertex model v;
      check_lorentz_reps_of_vertex model v)
    model.vertices;
  SMap.iter (print_endline @@< Lorentz.to_string) model.lorentz;
  SMap.iter (print_endline @@< Parameter.to_string) model.parameters;
  SMap.iter (print_endline @@< Propagator.to_string) model.propagators;
  SMap.iter (print_endline @@< Decay.to_string) model.decays;
  SMap.iter
    (fun symbol c -> ignore (UFOx.Expr.of_string c.UFO_Coupling.value))
    model.couplings;
  SMap.iter
    (fun symbol d ->
      List.iter (fun (_, w) -> ignore (UFOx.Expr.of_string w)) d.Decay.widths)
    model.decays

module Model =
  struct

    type flavor = int
    type constant = string
    type gauge = unit

    module M = Modeltools.Mutable
        (struct type f = flavor type g = gauge type c = constant end)

    let flavors = M.flavors
    let external_flavors = M.external_flavors
    let external_flavors = M.external_flavors
    let lorentz = M.lorentz
    let color = M.color
    let propagator = M.propagator
    let width = M.width
    let goldstone = M.goldstone
    let conjugate = M.conjugate
    let fermion = M.fermion
    let vertices = M.vertices
    let fuse2 = M.fuse2
    let fuse3 = M.fuse3
    let fuse = M.fuse
    let max_degree = M.max_degree
    let parameters = M.parameters
    let flavor_of_string = M.flavor_of_string
    let flavor_to_string = M.flavor_to_string
    let flavor_to_TeX = M.flavor_to_TeX
    let flavor_symbol = M.flavor_symbol
    let gauge_symbol = M.gauge_symbol
    let pdg = M.pdg
    let mass_symbol = M.mass_symbol
    let width_symbol = M.width_symbol
    let constant_symbol = M.constant_symbol
    module Ch = M.Ch
    let charges = M.charges

    let rcs = rcs_file

    let rec fermion_of_lorentz = function
      | Coupling.Spinor -> 1
      | Coupling.ConjSpinor -> -1
      | Coupling.Majorana -> 1
      | Coupling.Maj_Ghost -> 1
      | Coupling.Vectorspinor -> 1
      | Coupling.Vector | Coupling.Massive_Vector -> 0
      | Coupling.Scalar | Coupling.Tensor_1 | Coupling.Tensor_2 -> 0 
      | Coupling.BRS f -> fermion_of_lorentz f

    let rec conjugate_lorentz = function
      | Coupling.Spinor -> Coupling.ConjSpinor
      | Coupling.ConjSpinor -> Coupling.Spinor
      | Coupling.BRS f -> Coupling.BRS (conjugate_lorentz f) 
      | f -> f

    let propagator_of_lorentz = function
      | Coupling.Scalar -> Coupling.Prop_Scalar
      | Coupling.Spinor -> Coupling.Prop_Spinor
      | Coupling.ConjSpinor -> Coupling.Prop_ConjSpinor
      | Coupling.Majorana -> Coupling.Prop_Majorana
      | Coupling.Maj_Ghost ->
          invalid_arg "propagator_of_lorentz: SUSY ghosts do not propagate"
      | Coupling.Vector -> Coupling.Prop_Feynman
      | Coupling.Massive_Vector -> Coupling.Prop_Unitarity
      | Coupling.Vectorspinor -> 
          invalid_arg "propagator_of_lorentz: Vectorspinor"
      | Coupling.Tensor_1 ->
          invalid_arg "propagator_of_lorentz: Tensor_1"
      | Coupling.Tensor_2 ->
          invalid_arg "propagator_of_lorentz: Tensor_2"
      | Coupling.BRS _ ->
          invalid_arg "propagator_of_lorentz: no BRST"

    module F = Modeltools.Fusions (struct
      type f = flavor
      type c = constant
      let compare = compare
      let conjugate = conjugate
    end)

    module Q = UFOx.Q

    let dummy_tensor3 = Coupling.Scalar_Scalar_Scalar 1
    let dummy_tensor4 = Coupling.Scalar4 1
    let dummy_constant = "{coupling}"

    let third i j =
      match i, j with
      | 1, 2 | 2, 1 -> 3
      | 2, 3 | 3, 2 -> 1
      | 3, 1 | 1, 3 -> 2
      | _ -> invalid_arg "UFO.third"

    let coeff q1 q2 =
      Q.to_integer (Q.mul q1 q2)

    let translate_color3_1_1 c =
      let open UFOx.Color_Atom in
      match c with
      | Identity (i, j) -> 1
      | T (a, i, j) -> 1
      | F (a, b, c) -> Combinatorics.sign compare [a;b;c]
      | D (a, b, c) -> invalid_arg "d-tensor not supported yet"
      | Epsilon (i, j, k) -> invalid_arg "epsilon-tensor not supported yet"
      | EpsilonBar (i, j, k) -> invalid_arg "epsilon-tensor not supported yet"
      | T6 (a, i, j) -> invalid_arg "T6-tensor not supported yet"
      | K6 (i, j, k) -> invalid_arg "K6-tensor not supported yet"
      | K6Bar (i, j, k) -> invalid_arg "K6-tensor not supported yet"

    let translate_color3_1 c =
      match c with
      | [ ([], q) ] -> Q.to_integer q
      | [ ([c1], q) ] ->
	 Q.to_integer q * translate_color3_1_1 c1
      | [] -> invalid_arg "translate_color3_1: empty"
      | _ -> invalid_arg "translate_color3_1: sums of tensors not supported yet"

    let translate_color3 = function
      | [| c |] -> translate_color3_1 c
      | c ->
	 invalid_arg
	   (Printf.sprintf
	      "translate_color3: #color structures: %d > 1" (Array.length c))

    type color4 =
      | C3 of Q.t
      | F_F of Q.t * (int * int) * (int * int)

    let translate_color4_1_1 c =
      Q.make (translate_color3_1_1 c) 1

    let translate_color4_88 abc abc' =
      match ThoList.common abc abc' with
      | [] -> invalid_arg "translate_color4_88: not summation index"
      | [s] ->
	 if s >= 1 && s <= 4 then
	   invalid_arg "translate_color4_88: invalid summation index"
	 else
	   let order i i' =
	     if i = s then
	       -1
	     else if i' = s then
	       1
	     else
	       compare i i' in
	   begin match (Combinatorics.sort_signed order abc,
			Combinatorics.sort_signed order abc') with
	   | (eps, [_; b; c]), (eps', [_; b'; c']) ->
	      (eps * eps', (b, c), (b', c'))
	   | _ -> failwith "translate_color4_88: can't happen"
	   end
      | _ ->
	 invalid_arg "translate_color4_88: multiple summation indices"

    let translate_color4_1_2 c1 c2 =
      let open UFOx.Color_Atom in
      match c1, c2 with
      | Identity (i, j), Identity (i', l') ->
	 invalid_arg "quartic 3-3bar-couplings not supported yet"
      | T (a, i, j), T (a', i', j') ->
	 invalid_arg "quartic 3-3bar-couplings not supported yet"
      | F (a, b, c), F (a', b', c') ->
	 translate_color4_88 [a; b; c] [a'; b'; c']
      | T (a, i, j), F (a', b', c')
      | F (a', b', c'), T (a, i, j) ->
	 invalid_arg "quartic 8-8-3-3bar-couplings not supported yet"
      | Identity (i, j), T (a', i', l')
      | T (a', i', l'), Identity (i, j) ->
	 invalid_arg "open index"
      | Identity (i, j), F (a', b', c')
      | F (a', b', c'), Identity (i, j) ->
	 invalid_arg "open index"
      | D (a, b, c), _ | _, D (a, b, c) ->
	 invalid_arg "d-tensor not supported yet"
      | Epsilon (i, j, k), _ | _, Epsilon (i, j, k)
      | EpsilonBar (i, j, k), _| _, EpsilonBar (i, j, k) ->
	 invalid_arg "epsilon-tensor not supported yet"
      | T6 (a, i, j), _ | _, T6 (a, i, j) ->
	 invalid_arg "T6-tensor not supported yet"
      | K6 (i, j, k), _ | _, K6 (i, j, k)
      | K6Bar (i, j, k), _ | _, K6Bar (i, j, k) ->
	 invalid_arg "K6-tensor not supported yet"

    let translate_color4_1 c =
      match c with
      | [ ([], q) ] -> C3 (q)
      | [ ([c1], q) ] ->
	 C3 (Q.mul q (translate_color4_1_1 c1))
      | [ ([c1; c2], q) ] ->
	 let eps, bc, bc' = translate_color4_1_2 c1 c2 in
	 F_F (Q.mul q (Q.make eps 1), bc, bc')
      | _ -> invalid_arg "translate_color4_1: too many atoms"

    let translate_color4 c =
      match Array.map translate_color4_1 c with
      | [| C3 (q) |] -> q
      | [| F_F (q1, (b1, c1), (b1', c1'));
           F_F (q2, (b2, c2), (b2', c2'));
           F_F (q3, (b3, c3), (b3', c3')) |] -> q1
      | c ->
	 invalid_arg
	   (Printf.sprintf
	      "translate_color4: #color structures: %d" (Array.length c))

    let translate_coupling3 model p t c g =
      let module L = UFOx.Lorentz_Atom in
      let module C = UFOx.Color_Atom in
      match t, c, g with
      | [| [ [], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(0), p.(1), p.(2)),
	  Coupling.Scalar_Scalar_Scalar (Q.to_integer (Q.mul qt qc)),
	  dummy_constant)
      | [| [ [L.ProjP(i,j)], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(pred i), p.(pred (third i j)), p.(pred j)),
	  Coupling.FBF (coeff qt qc,
			Coupling.Psibar, Coupling.SR, Coupling.Psi),
	  dummy_constant)
      | [| [ [L.ProjM(i,j)], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(pred i), p.(pred (third i j)), p.(pred j)),
	  Coupling.FBF (coeff qt qc,
			Coupling.Psibar, Coupling.SL, Coupling.Psi),
	  dummy_constant)
      | [| [ ([L.ProjM(i,j)], qm);
	     ([L.ProjP(i',j')], qp)] |] as t,
        [| [ [], qc] |],
        [| [| g |] |] ->
	 if i = i' && j = j' then begin
	   if Q.is_null (Q.add qm qp) then 
	     ((p.(pred i), p.(pred (third i j)), p.(pred j)),
	      Coupling.FBF (coeff qp qc,
			    Coupling.Psibar, Coupling.P, Coupling.Psi),
	      dummy_constant)
	   else if Q.is_null (Q.sub qp qp) then 
	     ((p.(pred i), p.(pred (third i j)), p.(pred j)),
	      Coupling.FBF (coeff qp qc,
			    Coupling.Psibar, Coupling.S, Coupling.Psi),
	      dummy_constant)
	   else begin
	     prerr_endline
	       ("unhandled colorless vertex: " ^
		   (String.concat ", "
		      (List.map UFOx.Lorentz.to_string (Array.to_list t))));
	     ((p.(0), p.(1), p.(2)), dummy_tensor3, dummy_constant)
	   end
         end else
           invalid_arg "translate_coupling3: mismatched indices"
      | [| [ [L.Gamma(mu,i,j)], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(pred i), p.(pred mu), p.(pred j)),
	  Coupling.FBF (coeff qt qc,
			Coupling.Psibar, Coupling.V, Coupling.Psi),
	  dummy_constant)
      | [| [ [L.Gamma(mu,i,-1); L.ProjP(-1,j)], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(pred i), p.(pred mu), p.(pred j)),
	  Coupling.FBF (coeff qt qc,
			Coupling.Psibar, Coupling.VR, Coupling.Psi),
	  dummy_constant)
      | [| [ [L.Gamma(mu,i,-1); L.ProjM(-1,j)], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(pred i), p.(pred mu), p.(pred j)),
	  Coupling.FBF (coeff qt qc,
			Coupling.Psibar, Coupling.VL, Coupling.Psi),
	  dummy_constant)
      | [| [ [L.Metric(i,j)], qt] |],
        [| [ [], qc] |],
        [| [| g |] |] ->
	 ((p.(pred (third i j)), p.(pred i), p.(pred j)),
	  Coupling.Scalar_Vector_Vector (coeff qt qc),
	  dummy_constant)
      | [| [ ([L.P(mu,i)], q1);
	     ([L.P(mu',j')], q2)] |] as t,
        [| [ [], qc] |],
        [| [| g |] |] ->
	 prerr_endline
	   ("not yet handled colorless vertex: " ^
	       (String.concat ", "
		  (List.map UFOx.Lorentz.to_string (Array.to_list t))));
	 ((p.(0), p.(1), p.(2)), dummy_tensor3, dummy_constant)
      | [| [ ([L.Metric(ka1,la1); L.P(mu1,i1)], q1);
	     ([L.Metric(ka2,la2); L.P(mu2,i2)], q2);
	     ([L.Metric(ka3,la3); L.P(mu3,i3)], q3);
	     ([L.Metric(ka4,la4); L.P(mu4,i4)], q4);
	     ([L.Metric(ka5,la5); L.P(mu5,i5)], q5);
	     ([L.Metric(ka6,la6); L.P(mu6,i6)], q6)] |] as t,
        [| [ [], qc] |],
        [| [| g |] |] ->
	 prerr_endline
	   ("not yet handled colorless vertex: " ^
	       (String.concat ", "
		  (List.map UFOx.Lorentz.to_string (Array.to_list t))));
	 ((p.(0), p.(1), p.(2)), dummy_tensor3, dummy_constant)
      | t,
        [| [ [], qc] |],
        g ->
	 prerr_endline
	   ("unhandled colorless vertex: " ^
	       (String.concat ", "
		  (List.map UFOx.Lorentz.to_string (Array.to_list t))));
	 ((p.(0), p.(1), p.(2)), dummy_tensor3, dummy_constant)
      | [| t |], [| c |], [| [| g |] |] ->
	 prerr_endline
	   ("unhandled colorfull vertex: " ^ UFOx.Lorentz.to_string t);
	 ((p.(0), p.(1), p.(2)), dummy_tensor3, dummy_constant)
      | [| t |], [| c |], _->
	 invalid_arg "translate_coupling3: too many constants"
      | t, c, g ->
	 prerr_endline
	   ("unhandled colorfull vertex: " ^
	       (String.concat ", "
		  (List.map UFOx.Lorentz.to_string (Array.to_list t))));
	 ((p.(0), p.(1), p.(2)), dummy_tensor3, dummy_constant)

    let translate_coupling4 model p t c g =
      let module L = UFOx.Lorentz_Atom in
      let module C = UFOx.Color_Atom in
      match t, translate_color4 c, g with
      | [| [ [], qt] |], qc, [| [| g |] |] ->
	 ((p.(0), p.(1), p.(2), p.(3)),
	  Coupling.Scalar4 (coeff qt qc),
	  dummy_constant)
      | [| t |], qc, [| [| g |] |] ->
	 prerr_endline
	   ("unhandled vertex: " ^ UFOx.Lorentz.to_string t);
	 ((p.(0), p.(1), p.(2), p.(3)), dummy_tensor4, dummy_constant)
      | [| t |], qc, _->
	 invalid_arg "translate_coupling4: too many constants"
      | t, qc, g ->
	 prerr_endline
	   ("unhandled vertex: " ^
	       (String.concat ", "
		  (List.map UFOx.Lorentz.to_string (Array.to_list t))));
	 ((p.(0), p.(1), p.(2), p.(3)), dummy_tensor4, dummy_constant)

    let lorentz_of_symbol model symbol =
      try
	SMap.find symbol model.lorentz
      with
      | Not_found -> invalid_arg ("lorentz_of_symbol: " ^ symbol)

    let coupling_of_symbol model = function
      | None -> None
      | Some symbol ->
	 begin
	   try
	     Some (SMap.find symbol model.couplings)
	   with
	   | Not_found -> invalid_arg ("coupling_of_symbol: " ^ symbol)
	 end

    let translate_vertices model flavor_of_symbol model =
      List.fold_left (fun (v3, v4, vn) v ->
	let p = Array.map flavor_of_symbol v.Vertex.particles
	and g =
	  Array.map (Array.map (coupling_of_symbol model)) v.Vertex.couplings
	and t = Array.map (lorentz_of_symbol model) v.Vertex.lorentz
	and c = v.Vertex.color in
	let t = Array.map (fun l -> l.Lorentz.structure) t in
	match Array.length p with
	| 3 ->
	   let p, t, g = translate_coupling3 model p t c g in
           ((p, t, g) :: v3, v4, vn)
	| 4 ->
	   let p, t, g = translate_coupling4 model p t c g in
           (v3, (p, t, g) :: v4, vn)
	| _ -> invalid_arg "UFO.Model.init: only 3- and 4-vertices for now!")
        ([], [], []) (values model.vertices)

    let ufo_directory = ref Config.default_UFO_dir

    let dump_raw = ref false

    let propagator_of_lorentz = function
      | Coupling.Scalar -> Coupling.Prop_Scalar
      | Coupling.Spinor -> Coupling.Prop_Spinor
      | Coupling.ConjSpinor -> Coupling.Prop_ConjSpinor
      | Coupling.Majorana -> Coupling.Prop_Majorana
      | Coupling.Maj_Ghost -> invalid_arg 
         "UFO.Model.propagator_of_lorentz: SUSY ghosts do not propagate"
      | Coupling.Vector -> Coupling.Prop_Feynman
      | Coupling.Massive_Vector -> Coupling.Prop_Unitarity
      | Coupling.Vectorspinor -> invalid_arg
	 "UFO.Model.propagator_of_lorentz: Vectorspinor"
      | Coupling.Tensor_1 -> invalid_arg
	 "UFO.Model.propagator_of_lorentz: Tensor_1"
      | Coupling.Tensor_2 -> invalid_arg
	 "UFO.Model.propagator_of_lorentz: Tensor_2"
      | Coupling.BRS _ -> invalid_arg
	 "UFO.Model.propagator_of_lorentz: no BRST"

    let conjugate_of_particle_array particles =
      Array.init
	(Array.length particles)
	(fun i ->
	  let f' = Particle.conjugate particles.(i) in
	  match ThoArray.match_all f' particles with
	  | [i'] -> i'
	  | [] ->
	     invalid_arg ("no charge conjugate: " ^ f'.Particle.name)
	  | _ ->
	     invalid_arg ("multiple charge conjugates: " ^ f'.Particle.name))

    let invert_array a =
      let table = SHash.create 37 in
      Array.iteri (fun i s -> SHash.add table s i) a;
      (fun name ->
	try
	  SHash.find table name
	with
	| Not_found -> invalid_arg ("not found: " ^ name))

    let init () =
      let model = parse_directory !ufo_directory in
      let model =
	let is_unphysical = not @< Particle.is_physical in
	let particles' =
	  Particle.filter Particle.is_physical model.particles in
	let vertices' =
	  Vertex.filter
	    (not @< (Vertex.contains model.particles is_unphysical))
	    model.vertices in
	{ model with particles = particles'; vertices = vertices' } in
      if !dump_raw then
	dump model;
      let particle_array = Array.of_list (values model.particles) in
      let flavors = ThoList.range 0 (Array.length particle_array - 1) in
      let name_array = Array.map (fun f -> f.Particle.name) particle_array in
      let flavor_of_string = invert_array name_array in
      let symbol_array = Array.of_list (keys model.particles) in
      let flavor_of_symbol = invert_array symbol_array in
      let conjugate_array = conjugate_of_particle_array particle_array in
      let conjugate f = conjugate_array.(f) in
      let functions = [] in
      let variables = [] in
      let (vertices3, vertices4, verticesn) as vertices =
	translate_vertices model flavor_of_symbol model in
      let max_degree = match vertices4 with [] -> 3 | _ -> 4 in
      let input_parameters = 
        ("0.0_default", 0.0) ::
        (List.map (fun (n, v, _) -> (n, v)) variables) in
      let derived_parameters =
        List.map (fun (n, f, _) -> (Coupling.Real n, Coupling.Const 0))
          functions in
      let particle f = particle_array.(f)
      and flavor_symbol f = symbol_array.(f) in
      let pdg f = (particle f).Particle.pdg_code
      and color f = UFOx.Color.omega (particle f).Particle.color
      and lorentz f = UFOx.Lorentz.omega (particle f).Particle.spin in
      let propagator f = propagator_of_lorentz (lorentz f)
      and fermion f = fermion_of_lorentz (lorentz f) in
      let flavor_to_string f = (particle f).Particle.name
      and flavor_to_TeX f = (particle f).Particle.texname in
      let mass_symbol f = (particle f).Particle.mass
      and width_symbol f = (particle f).Particle.width in
      let gauge_symbol () = "?GAUGE?" in
      let color f = Color.Singlet in (* TEMPORARY HACK! *)
      M.setup ~color ~pdg ~lorentz ~propagator
        ~width:(fun f -> Coupling.Constant)
        ~goldstone:(fun f -> None)
        ~conjugate ~fermion ~max_degree ~vertices
        ~flavors:[("All Flavors", flavors)]
        ~parameters:(fun () ->
          { Coupling.input = input_parameters;
            Coupling.derived = derived_parameters;
            Coupling.derived_arrays = [] })
        ~flavor_of_string ~flavor_to_string ~flavor_to_TeX
        ~flavor_symbol ~gauge_symbol ~mass_symbol ~width_symbol
        ~constant_symbol:(fun c -> "g")

    let load () =
      init ()

    let options = Options.create
        [ ("UFO_dir", Arg.String (fun name -> ufo_directory := name),
           "UFO model directory (default: " ^ !ufo_directory ^ ")");
          ("dump", Arg.Set dump_raw, "dump UFO model");
          ("exec", Arg.Unit load,
           "load the model files (required _before_ any particle)");
          ("help", Arg.Unit (fun () -> prerr_endline "..."),
           "print information on the model")]

  end

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

