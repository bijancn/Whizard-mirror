(* $Id: model_file.ml 4015 2013-01-03 16:04:18Z jr_reuter $

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

(* In this module, the label [[v]] is ubiquitous for an optional
   ``verbose'' flag. *)

open Printf

(* \thocwmodulesubsection{Parsing} *)

let model_of_channel channel =
  try
    Model_file_parser.file Model_file_lexer.token (Lexing.from_channel channel)
  with
  | Model_file_syntax.Syntax_Error (msg, i, j) ->
      invalid_arg (sprintf "syntax error (%s) at: [%d,%d]" msg i j)

let model_of_file = function
  | "-" -> model_of_channel stdin
  | name ->
      let channel = open_in name in
      let model = model_of_channel channel in
      close_in channel;
      model

type error_level = Info | Warning | Error | Panic
let error_level_to_string = function
  | Info -> "INFO"
  | Warning -> "WARNING"
  | Error -> "ERROR"
  | Panic -> "PANIC"

let error ?(v = false) ?pfx ?(lvl = Error) msg =
  if v then begin
    begin match pfx with
    | Some pfx -> eprintf "%s: " pfx
    | None -> ()
    end;
    eprintf "%s: %s\n" (error_level_to_string lvl) msg
  end

(* \thocwmodulesubsection{Metadata} *)

type metadata =
    { name : string;
      version : string option;
      authors : string list;
      created : string option;
      revised : string list }

(* Printing metadata and adding defaults, if necessary. *)

let print_metadata md =
  printf "%% %s -- O'Mega model description file\n" md.name;
  begin match md.version with
  | None -> printf "version { %cId:%c } %% missing in input file\n" '$' '$';
  | Some version -> printf "version {%s}\n" version
  end;
  begin match md.authors with
  | [] -> printf "%% author missing in input file\n";
  | authors -> List.iter (fun a -> printf "author  {%s}\n" a) authors;
  end;
  begin match md.created with
  | None -> printf "%% creation date missing in input file\n";
  | Some created -> printf "created {%s}\n" created
  end;
  List.iter (fun r -> printf "revised {%s}\n" r) md.revised

(* Extract metadata from the abstract syntax ``tree'', dropping duplicate data. *)

let extract_authors ?(v = false) ?pfx = function
  | [] ->
      error ~v ?pfx ~lvl:Warning "no author in model file!";
      []
  | rev_authors -> List.rev rev_authors

let extract_version ?(v = false) ?pfx = function
  | [] ->
      error ~v ?pfx ~lvl:Warning "no version in model file!";
      None
  | [version] -> Some version
  | version :: _ -> 
      error ~v ?pfx ~lvl:Warning "multiple versions in model file!";
      error ~v ?pfx ~lvl:Info "keeping the last version.";
      Some version

let extract_created ?(v = false) ?pfx rev_created =
  match List.rev rev_created with
  | [] ->
      error ~v ?pfx ~lvl:Warning "no creation date in model file!";
      None
  | [created] -> Some created
  | created :: _ -> 
      error ~v ?pfx ~lvl:Warning "multiple creation dates in model file!";
      error ~v ?pfx ~lvl:Info "keeping the first date.";
      Some created

let extract_metadata ?v name file =
  { name = name;
    authors = extract_authors ?v ~pfx:name file.Model_file_syntax.authors;
    version = extract_version ?v ~pfx:name file.Model_file_syntax.version;
    created = extract_created ?v ~pfx:name file.Model_file_syntax.created;
    revised = List.rev file.Model_file_syntax.revised }

(* \thocwmodulesubsection{Particles} *)

type particle =
    { name : string;
      is_anti : bool;
      lorentz : Coupling.lorentz;
      fermion : int;
      charge : int option;
      color : int option;
      pdg : int option;
      tex : string option }

let print_opt_pdg name = function
  | None -> ()
  | Some pdg -> printf "%% %s : pdg = %d\n" name pdg

let print_neutral p =
  printf "particle %s : ... \n" p.name;
  print_opt_pdg p.name p.pdg
    
let print_charged p a =
  printf "particle %s %s : ... \n" p.name a.name;
  print_opt_pdg p.name p.pdg;
  print_opt_pdg a.name a.pdg

let print_particle = function
  | (p, None) -> print_neutral p
  | (p, Some a) -> if not p.is_anti then print_charged p a

module SMap =
  Map.Make (struct type t = string let compare = compare end)

type particles = (particle * particle option) SMap.t

let add_neutral name particle map =
  SMap.add name (particle, None) map

let add_charged name1 particle1 name2 particle2 map =
  SMap.add name1 (particle1, Some particle2)
    (SMap.add name2 (particle2, Some particle1) map)

(* Boolean values default to [[false]]. *)

let boolean_attrib ?v ?pfx name attribs =
  try
    match String.lowercase (List.assoc name attribs) with
    | "true" | "t" | "1" -> true
    | "false" | "f" | "0" -> false
    | value ->
        error ?v ?pfx ("invalid boolean value for `" ^ name ^ "': `" ^ value ^ "'!");
        error ?v ?pfx ~lvl:Info "assuming false.";
        false
  with
  | Not_found -> false

let opt_attrib name attribs =
  try Some (List.assoc name attribs) with Not_found -> None

let opt_int_attrib ?v ?pfx name attribs =
  try
    Some (int_of_string (List.assoc name attribs))
  with
  | Not_found -> None
  | Failure "int_of_string" ->
      error ?v ?pfx ("invalid optional integer value for `" ^ name ^
                     "': `" ^ List.assoc name attribs ^ "'!");
      error ?v ?pfx ~lvl:Info "ignored.";
      None

(* Extract the lorentz representation from the \texttt{spin},
   \texttt{majorana} and \texttt{massive} attributes.  *)
let lorentz_of_attribs ?v ?pfx name is_anti attribs =
  try
    match List.assoc "spin" attribs with
    | "0" ->
        Coupling.Scalar
    | "1/2" ->
        if boolean_attrib "majorana" attribs then
          Coupling.Majorana
        else if is_anti then
          Coupling.ConjSpinor
        else
            Coupling.Spinor
    | "1" ->
        if boolean_attrib "massive" attribs then
          Coupling.Massive_Vector
        else
          Coupling.Vector
    | "2" ->
        Coupling.Tensor_2
    | s ->
        error ?v ?pfx ("invalid spin for particle `" ^ name ^ "': `" ^ s ^ "'!");
        error ?v ?pfx ~lvl:Info "assuming spin=0.";
        Coupling.Scalar
  with
  | Not_found ->
      error ?v ?pfx ("no spin given for particle `" ^ name ^ "'!");
      error ?v ?pfx ~lvl:Info "assuming spin=0.";
      Coupling.Scalar

let charge_of_attribs ?v ?pfx name is_anti attribs =
  try
    match List.assoc "spin" attribs with
    | "0" ->
        Coupling.Scalar
    | "1/2" ->
        if boolean_attrib "majorana" attribs then
          Coupling.Majorana
        else if is_anti then
          Coupling.ConjSpinor
        else
            Coupling.Spinor
    | "1" ->
        if boolean_attrib "massive" attribs then
          Coupling.Massive_Vector
        else
          Coupling.Vector
    | "2" ->
        Coupling.Tensor_2
    | s ->
        error ?v ?pfx ("invalid spin for particle `" ^ name ^ "': `" ^ s ^ "'!");
        error ?v ?pfx ~lvl:Info "assuming spin=0.";
        Coupling.Scalar
  with
  | Not_found ->
      error ?v ?pfx ("no spin given for particle `" ^ name ^ "'!");
      error ?v ?pfx ~lvl:Info "assuming spin=0.";
      Coupling.Scalar

let rec fermion_of_lorentz = function
  | Coupling.Scalar -> 0
  | Coupling.Spinor -> 1
  | Coupling.ConjSpinor -> -1
  | Coupling.Majorana -> 1
  | Coupling.Maj_Ghost -> 0
  | Coupling.Vector -> 0
  | Coupling.Massive_Vector -> 0
  | Coupling.Vectorspinor -> 1
  | Coupling.Tensor_1 -> 0
  | Coupling.Tensor_2 -> 0
  | Coupling.BRS lorentz -> fermion_of_lorentz lorentz

let fermion_of_attribs ?v ?pfx name is_anti attribs =
  match
    (boolean_attrib ?v ?pfx "fermion" attribs,
     boolean_attrib ?v ?pfx "boson" attribs) with
  | false, true -> 0
  | true, false -> if is_anti then 1 else -1
  | true, true ->
      error ?v ?pfx ("both `fermion' and `boson' given for `" ^ name ^ "'!");
      error ?v ?pfx ~lvl:Info "ignored.";
      fermion_of_lorentz (lorentz_of_attribs ?v ?pfx name is_anti attribs)
  | false, false ->
      fermion_of_lorentz (lorentz_of_attribs ?v ?pfx name is_anti attribs)

let particle_of_attribs ?v ?pfx name attribs =
  let lorentz = lorentz_of_attribs ?v ?pfx name false attribs in
  let fermion = fermion_of_attribs ?v ?pfx name false attribs in
  { name = name;
    is_anti = false;
    lorentz = lorentz;
    fermion = fermion;
    charge = opt_int_attrib ?v ?pfx "charge" attribs;
    color = opt_int_attrib ?v ?pfx "color" attribs;
    pdg = opt_int_attrib ?v ?pfx "pdg" attribs;
    tex = opt_attrib "tex" attribs }

let flip_opt_sign = function
  | None -> None
  | Some n -> Some (- n)

let color_opt_sign = function
  | None -> None
  | Some n when n = 3 || n = -3 -> Some (-n)
  | Some n -> Some n

let anti_particle_of_attribs ?v ?pfx name attribs =
  let lorentz = lorentz_of_attribs ?v ?pfx name true attribs in
  let fermion = fermion_of_attribs ?v ?pfx name true attribs in
  { name = name;
    is_anti = true;
    lorentz = lorentz;
    fermion = fermion;
    charge = flip_opt_sign (opt_int_attrib ?v ?pfx "charge" attribs);
    color = color_opt_sign (opt_int_attrib ?v ?pfx "color" attribs);
    pdg = flip_opt_sign (opt_int_attrib ?v ?pfx "pdg" attribs);
    tex = opt_attrib "tex.anti" attribs }

module SSet =
  Set.Make (struct type t = string let compare = compare end)

let known_attribs =
  List.fold_right SSet.add
    ["spin"; "massive"; "majorana"; "fermion"; "boson";
     "pdg"; "tex"; "tex.anti"; "charge"; "color"] SSet.empty

let scan_particle_attrib ?v ?pfx (name, value) =
  if not (SSet.mem name known_attribs) then begin
    error ?v ?pfx ("unknown particle attribute `" ^ name ^ "' = `" ^ value ^ "'!");
    error ?v ?pfx ~lvl:Info "ignored."
  end

let scan_particle_attribs ?v ?pfx attribs =
  List.iter (scan_particle_attrib ?v ?pfx) attribs

let add_particle ?v ?pfx raw_particle map =
  scan_particle_attribs ?v ?pfx raw_particle.Model_file_syntax.attribs;
  match raw_particle.Model_file_syntax.name with
  | Model_file_syntax.Neutral name ->
      add_neutral name (particle_of_attribs ?v ?pfx name
                          raw_particle.Model_file_syntax.attribs) map
  | Model_file_syntax.Charged (name, anti) ->
      add_charged
        name (particle_of_attribs ?v ?pfx name
                raw_particle.Model_file_syntax.attribs)
        anti (anti_particle_of_attribs ?v ?pfx anti
                raw_particle.Model_file_syntax.attribs)
        map

let extract_particles ?v name file =
  List.fold_right
    (add_particle ?v ~pfx:name)
    file.Model_file_syntax.particles SMap.empty

(* \thocwmodulesection{Test Program} *)

let _ =
  let file = "-" in
  let model = model_of_file file in
  let metadata = extract_metadata ~v:true file model in
  let particles = extract_particles ~v:true file model in
  let vertices = model.Model_file_syntax.vertices in
  print_metadata metadata;
  SMap.iter (fun name p -> print_particle p) particles;
  List.iter (fun v -> Vertex.process_vertex v.Model_file_syntax.expr) vertices

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
