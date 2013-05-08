(* $Id: vertex_syntax.ml 4015 2013-01-03 16:04:18Z jr_reuter $

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
      { token : t;
	super : t list;
	sub : t list }

    let plug = function
      | Digit _ as t -> [t]
      | Token _ as t -> [t]
      | Scripted _ as t -> [t]
      | List tl -> tl

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
    | Charge of Expr.t
    | Mass of Token.t list
    | Width of Token.t list

    type t =
      { name : name;
	attr : attr list }

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

  end

module File =
  struct

    type declaration =
    | Particle of Particle.t
    | Parameter of Parameter.t
    | Lagrangian of Expr.t * Token.t
    | Include of string

    type t = declaration list

    let empty = []

  end

module Model =
  struct

    type t =
      { particles : Particle.t list;
	parameters : Parameter.t list;
	lagrangian : (Expr.t * Token.t) list }

    let empty =
      { particles = [];
	parameters = [];
	lagrangian = [] }

    let l et lagrangian = { lagrangian with lagrangian = [et] }

  end
