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

module Token :
  sig

    type t =
    | Digit of int
    | Token of string
    | Scripted of scripted
    | List of t list

    and scripted = 
      { token : t;
	super : t list;
	sub : t list }

    val plug : t -> t list

  end

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
    | Function of Token.t * t list

    val integer : int -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t -> t
    val apply : Token.t -> t list -> t

  end

module Particle :
  sig

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
    | Charge of Expr.t
    | Mass of Token.t list
    | Width of Token.t list

    type t =
      { name : name;
	attr : attr list }

  end

module Parameter :
  sig

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

  end

module File_Tree :
  sig

    type declaration =
    | Particle of Particle.t
    | Parameter of Parameter.t
    | Lagrangian of Expr.t * Token.t
    | Include of string

    type t = declaration list

    val empty : t

  end

module File :
  sig

    type declaration =
    | Particle of Particle.t
    | Parameter of Parameter.t
    | Lagrangian of Expr.t * Token.t

    type t = declaration list

    val empty : t

    val expand_includes : (string -> File_Tree.t) -> File_Tree.t -> t

  end

module Model :
  sig

    type t =
      { particles : Particle.t list;
	parameters : Parameter.t list;
	lagrangian : (Expr.t * Token.t) list }

    val empty : t

  end
