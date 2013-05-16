(* $Id: vertex_syntax.ml 4276 2013-05-14 14:58:35Z ohl $

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

(* \thocwmodulesection{Abstract Syntax} *)

type coeff = int
type name = string
type momentum = int
type index = name

type field =
  { flavor : name;
    conjugate : bool;
    f_indices : index list }

type tensor =
  { t_name : name;
    t_indices : index list }

type t =
| Empty
| Field of field
| Momentum of momentum list * index
| Lorentz of tensor
| Color of tensor
| Product of t list
| Sum of (coeff * t) list

let null = Empty

exception Syntax_Error of string * int * int

type identifier =
| Id_Flavor
| Id_Momentum
| Id_Lorentz
| Id_Color
| Id_Index

module Token =
  struct

    type t =
    | Digit of int
    | Token of string
    | Scripted of scripted
    | List of t list

    and scripted = 
      { stem : t;
	prefix : string list;
	super : t list;
	sub : t list }

    let plug = function
      | List tl -> tl
      | _ as t -> [t]

    let digit i =
      Digit i

    let token s =
      Token s

    let list = function
      | [] -> List []
      | [Scripted {stem = t; prefix = []; super = []; sub = []}] -> t
      | [t] -> t
      | tl ->  List tl

    let optional = function
      | None -> []
      | Some t -> plug t

    let scripted prefix token (super, sub) =
      match token, prefix, super, sub with
      | _, [], None, None -> token
      | (Digit _ | Token _ | List _) as t, _, _, _ ->
	Scripted { stem = t;
		   prefix = prefix;
		   super = optional super;
		   sub = optional sub }
      | Scripted st, _, _, _ ->
	Scripted { stem = st.stem;
		   prefix = prefix @ st.prefix;
		   super = st.super @ optional super;
		   sub = st.sub @ optional sub }

    let wrap_scripted = function
      | Scripted st -> st
      | t ->  { stem = t; prefix = []; super = []; sub = [] }

    let rec stem = function
      | Digit _ | Token _ as t -> t
      | Scripted { stem = t } -> stem t
      | List tl ->
	begin match List.rev tl with
	| [] -> List []
	| t :: _ -> stem t
	end

    (* Strip superfluous [List] and [Scripted] constructors. *)
    (* NB: This might be unnecessary, if we used smart constructors. *)

    let rec strip = function
      | Digit _ | Token _ as t -> t
      | Scripted { stem = t; prefix = []; super = []; sub = [] } -> strip t
      | Scripted { stem = t; prefix = prefix; super = super; sub = sub } ->
	Scripted { stem = strip t;
		   prefix = prefix;
		   super = List.map strip super;
		   sub = List.map strip sub }
      | List tl ->
	begin match List.map strip tl with
	| [] -> List []
	| [t] -> t
	| tl ->  List tl
	end

    (* Recursively merge nested [List] and [Scripted] constructors. *)
    (* NB: This might be unnecessary, if we used smart constructors. *)

    let rec flatten = function
      | Digit _ | Token _ as t -> t
      | List tl -> flatten_list tl
      | Scripted st -> flatten_scripted st

    and flatten_list tl =
      match List.map flatten tl with
      | [] -> List []
      | [t] -> t
      | tl ->  List tl

    and flatten_scripted = function
      | { stem = t; prefix = []; super = []; sub = [] } -> t
      | { stem = t; prefix = prefix; super = super; sub = sub } ->
	let super = List.map flatten super
	and sub = List.map flatten sub in
	begin match flatten t with
	| Digit _ | Token _ | List _ as t ->
	  Scripted { stem = t;
		     prefix = prefix;
		     super = super;
		     sub = sub }
	| Scripted st ->
	  Scripted { stem = st.stem;
		     prefix = prefix @ st.prefix;
		     super = st.super @ super;
		     sub = st.sub @ sub }
	end

    let ascii_A = Char.code 'A'
    let ascii_Z = Char.code 'Z'
    let ascii_a = Char.code 'a'
    let ascii_z = Char.code 'z'

    let is_char c =
      let a = Char.code c in
      (ascii_A <= a && a <= ascii_Z) || (ascii_a <= a && a <= ascii_z)

    let is_backslash c =
      c = '\\'

    let first_char s =
      s.[0]

    let last_char s =
      s.[String.length s - 1]

    let rec to_string = function
      | Digit i -> string_of_int i
      | Token s -> s
      | Scripted t -> scripted_to_string t
      | List tl -> "{" ^ list_to_string tl ^ "}"

    and list_to_string = function
      | [] -> ""
      | [Scripted { stem = t; super = []; sub = [] }] -> to_string t
      | [Scripted _ as t] -> "{" ^ to_string t ^ "}"
      | [t] -> to_string t
      | tl -> "{" ^ concat_tokens tl ^ "}"

    and scripted_to_string t =
      let super =
	match t.super with
	| [] -> ""
	| tl -> "^" ^ list_to_string tl
      and sub =
	match t.sub with
	| [] -> ""
	| tl -> "_" ^ list_to_string tl in
      String.concat "" t.prefix ^ to_string t.stem ^ super ^ sub

    and required_space t1 t2 =
      let required_space' s1 s2 =
	if is_backslash (first_char s2) then
	  []
	else if is_backslash (first_char s1) && is_char (last_char s1) then
	  [Token " "]
	else
	  [] in
      match t1, t2 with
      | Token s1, Token s2 -> required_space' s1 s2
      | Scripted s1, Token s2 -> required_space' (scripted_to_string s1) s2
      | Token s1, Scripted s2 -> required_space' s1 (scripted_to_string s2)
      | Scripted s1, Scripted s2 ->
	required_space' (scripted_to_string s1) (scripted_to_string s2)
      | List _, _ | _, List _ | _, Digit _ | Digit _, _ -> []

    and interleave_spaces tl =
      ThoList.interleave_nearest required_space tl

    and concat_tokens tl =
      String.concat "" (List.map to_string (interleave_spaces tl)) 

    let	compare t1 t2 =
      Pervasives.compare t1 t2

  end

module Expr =
  struct

    type t =
    | Integer of int
    | Sum of t list
    | Diff of t * t
    | Product of t list
    | Ratio of t * t
    | Function of Token.t * t list

    let integer i = Integer i

    let rec add a b =
      match a, b with
      | Integer a, Integer b -> Integer (a + b)
      | Sum a, Sum b -> Sum (a @ b)
      | Sum a, b -> Sum (a @ [b])
      | a, Sum b -> Sum (a :: b)
      | a, b -> Sum ([a; b])

    (* (a1 - a2) - (b1 - b2) = (a1 + b2) - (a2 + b1) *)
    (* (a1 - a2) - b = a1 - (a2 + b) *)
    (* a - (b1 - b2) = (a + b2) - b1 *)

    and sub a b =
      match a, b with
      | Integer a, Integer b -> Integer (a - b)
      | Diff (a1, a2), Diff (b1, b2) -> Diff (add a1 b2, add a2 b1)
      | Diff (a1, a2), b -> Diff (a1, add a2 b)
      | a, Diff (b1, b2) -> Diff (add a b2, b1)	
      | a, b -> Diff (a, b)	

    and mult a b =
      match a, b with
      | Integer a, Integer b -> Integer (a * b)
      | Product a, Product b -> Product (a @ b)
      | Product a, b -> Product (a @ [b])
      | a, Product b -> Product (a :: b)
      | a, b -> Product ([a; b])

    and div a b =
      match a, b with
      | Ratio (a1, a2), Ratio (b1, b2) -> Ratio (mult a1 b2, mult a2 b1)
      | Ratio (a1, a2), b -> Ratio (a1, mult a2 b)
      | a, Ratio (b1, b2) -> Ratio (mult a b2, b1)	
      | a, b -> Ratio (a, b)	

    let apply f args =
      Function (f, args)

    let rec to_string = function
      | Integer i -> string_of_int i
      | Sum ts -> String.concat "+" (List.map to_string ts)
      | Diff (t1, t2) -> to_string t1 ^ "-" ^ to_string t2
      | Product ts -> String.concat "*" (List.map to_string ts)
      | Ratio (t1, t2) -> to_string t1 ^ "/" ^ to_string t2
      | Function (f, args) ->
	Token.to_string f ^
	  String.concat ""
	  (List.map (fun arg -> "{" ^ to_string arg ^ "}") args)

  end

module Particle =
  struct

    type name =
    | Neutral of Token.t list
    | Charged of Token.t list * Token.t list

    type attr =
    | TeX of Token.t list
    | TeX_Anti of Token.t list
    | Alias of Token.t list
    | Alias_Anti of Token.t list
    | Fortran of Token.t list
    | Fortran_Anti of Token.t list
    | Spin of Expr.t
    | Color of Token.t list
    | Charge of Expr.t
    | Mass of Token.t list
    | Width of Token.t list

    type t =
      { name : name;
	attr : attr list }

    let name_to_string = function
      | Neutral p ->
	"\\neutral{" ^ Token.list_to_string p ^ "}"
      | Charged (p, ap) ->
	"\\charged{" ^ Token.list_to_string p ^
	  "}{" ^ Token.list_to_string ap ^ "}"

    let attr_to_string = function
      | TeX tl -> "\\TeX{" ^ Token.list_to_string tl ^ "}"
      | TeX_Anti tl -> "\\anti\\TeX{" ^ Token.list_to_string tl ^ "}"
      | Alias tl -> "\\alias{" ^ Token.list_to_string tl ^ "}"
      | Alias_Anti tl -> "\\anti\\alias{" ^ Token.list_to_string tl ^ "}"
      | Fortran tl -> "\\fortran{" ^ Token.list_to_string tl ^ "}"
      | Fortran_Anti tl -> "\\anti\\fortran{" ^ Token.list_to_string tl ^ "}"
      | Spin e -> "\\spin{" ^ Expr.to_string e ^ "}"
      | Color tl -> "\\color{" ^ Token.list_to_string tl ^ "}"
      | Charge e -> "\\charge{" ^ Expr.to_string e ^ "}"
      | Mass tl -> "\\mass{" ^ Token.list_to_string tl ^ "}"
      | Width tl -> "\\width{" ^ Token.list_to_string tl ^ "}"

    let to_string p =
      name_to_string p.name ^
	String.concat "" (List.map attr_to_string p.attr)
	
  end

module Parameter =
  struct

    type attr =
    | TeX of Token.t list
    | Alias of Token.t list
    | Fortran of Token.t list

    type t' =
      { name : Token.t list;
	value : Expr.t;
	attr : attr list}

    type t =
    | Input of t'
    | Derived of t'

    let attr_to_string = function
      | TeX tl -> "\\TeX{" ^ Token.list_to_string tl ^ "}"
      | Alias tl -> "\\alias{" ^ Token.list_to_string tl ^ "}"
      | Fortran tl -> "\\fortran{" ^ Token.list_to_string tl ^ "}"

    let to_string' p =
      "{" ^ Token.list_to_string p.name ^ "}{" ^
	Expr.to_string p.value ^ "}" ^
	String.concat "" (List.map attr_to_string p.attr)

    let to_string = function
      | Input p -> "\\parameter" ^ to_string' p
      | Derived p -> "\\derived" ^ to_string' p

  end

module Color =
  struct

    type t =
    | U of int
    | SU of int
    | O of int
    | SO of int
    | Sp of int
    | E6 | E7 | E8
    | F4 | G2
      
    type r = int

  end

module Lorentz =
  struct

    type t =
    | Vector
    | Dirac
    | ConjDirac
    | Weyl
    | ConjWeyl

  end

module Index =
  struct

    type attr =
    | Color of Token.t list
    | Flavor of Token.t list
    | Lorentz of Token.t list

    type t =
      { name : Token.t list;
	attr : attr list }

    let attr_to_string = function
      | Color tl -> "\\color{" ^ Token.list_to_string tl ^ "}"
      | Flavor tl -> "\\flavor{" ^ Token.list_to_string tl ^ "}"
      | Lorentz tl -> "\\lorentz{" ^ Token.list_to_string tl ^ "}"

    let to_string i =
      "\\index{" ^ Token.list_to_string i.name ^ "}" ^
	String.concat "" (List.map attr_to_string i.attr)
  end

module Tensor =
  struct

    type attr =
    | Color of Token.t list
    | Flavor of Token.t list
    | Lorentz of Token.t list

    type t =
      { name : Token.t list;
	attr : attr list }

    let attr_to_string = function
      | Color tl -> "\\color{" ^ Token.list_to_string tl ^ "}"
      | Flavor tl -> "\\flavor{" ^ Token.list_to_string tl ^ "}"
      | Lorentz tl -> "\\lorentz{" ^ Token.list_to_string tl ^ "}"

    let to_string i =
      "\\tensor{" ^ Token.list_to_string i.name ^ "}" ^
	String.concat "" (List.map attr_to_string i.attr)
  end

module File_Tree =
  struct

    type declaration =
    | Particle of Particle.t
    | Parameter of Parameter.t
    | Index of Index.t
    | Tensor of Tensor.t
    | Vertex of Expr.t * Token.t
    | Include of string

    type t = declaration list

    let empty = []

  end

module File =
  struct

    type declaration =
    | Particle of Particle.t
    | Parameter of Parameter.t
    | Index of Index.t
    | Tensor of Tensor.t
    | Vertex of Expr.t * Token.t

    type t = declaration list

    let empty = []

    let expand_includes parser unexpanded =
      let rec expand_includes' unexpanded expanded =
	List.fold_right (fun decl decls ->
	  match decl with
	  | File_Tree.Particle p -> Particle p :: decls
	  | File_Tree.Parameter p -> Parameter p :: decls
	  | File_Tree.Index i -> Index i :: decls
	  | File_Tree.Tensor t -> Tensor t :: decls
	  | File_Tree.Vertex (e, v) -> Vertex (e, v) :: decls
	  | File_Tree.Include f ->
	    expand_includes' (parser f) decls)
	  unexpanded expanded in
      expand_includes' unexpanded []

    let to_strings decls =
      List.map
	(function
	| Particle p -> Particle.to_string p
	| Parameter p -> Parameter.to_string p
	| Index i -> Index.to_string i
	| Tensor t -> Tensor.to_string t
	| Vertex (Expr.Integer 1, t) -> 
	  "\\vertex{" ^ Token.to_string t ^ "}"
	| Vertex (e, t) ->
	  "\\vertex[" ^ Expr.to_string e ^ "]{" ^
	    Token.to_string t ^ "}")
	decls

  end
