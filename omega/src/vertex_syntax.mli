(* $Id: vertex_syntax.mli 4015 2013-01-03 16:04:18Z jr_reuter $

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

val null : t

exception Syntax_Error of string * int * int

type identifier =
| Id_Flavor
| Id_Momentum
| Id_Lorentz
| Id_Color
| Id_Index

module Expr :
  sig

    (* Values (a.k.a. variables) are just functions with
       an empty argument list. *)

    type t =
    | Integer of int
    | Sum of t list
    | Diff of t * t
    | Product of t list
    | Ratio of t * t
    | Function of string * t list

    val integer : int -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t -> t
    val apply : string -> t list -> t

  end

type token =
| Digit of int
| Name of string
| Scripted of scripted
| List of token list

and scripted = 
  { token : token;
    super : token list;
    sub : token list }

val plug : token -> token list

module Particle :
  sig

    type t =
      { name : string;
	tex : string option }

  end

module Parameter :
  sig

    type t =
      { name : string;
	derived : Expr.t option;
	tex : string option }

  end

module Model :
  sig

    type t =
      { particles : Particle.t list;
	parameters : Parameter.t list;
	lagrangian : (Expr.t * token) list }

    val empty : t
    val l : Expr.t * token -> t -> t

  end
