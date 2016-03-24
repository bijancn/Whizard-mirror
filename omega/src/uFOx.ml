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
  let i = max 0 start_pos.Lexing.pos_cnum in
  let j = min (String.length text) (max (i + 1) end_pos.Lexing.pos_cnum) in
  String.sub text i (j - i)

let error_in_file name start_pos end_pos =
  Printf.sprintf
    "%s:%d.%d-%d.%d"
    name
    start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

let parse text =
  try
    UFOx_parser.input
      UFOx_lexer.token
      (UFOx_lexer.init_position "" (Lexing.from_string text))
  with
  | UFOx_syntax.Syntax_Error (msg, start_pos, end_pos) ->
     invalid_arg (Printf.sprintf "syntax error (%s) at: `%s'"
                    msg  (error_in_string text start_pos end_pos))
  | Parsing.Parse_error ->
     invalid_arg ("parse error: " ^ text)


let positive integers =
  List.filter (fun i -> i > 0) integers

module Q = Algebra.Small_Rational

module type Atomic_Tensor =
  sig
    type t
    val of_expr : string -> UFOx_syntax.expr list -> t
    val to_string : t -> string
    type index_classes
    val classify_indices : t list -> index_classes
  end

module type Tensor =
  sig
    type tensor
    type t = (tensor list * Q.t) list
    val of_expr : UFOx_syntax.expr -> t
    val to_string : t -> string
  end

module Tensor (A : Atomic_Tensor) : Tensor with type tensor = A.t =
  struct

    module S = UFOx_syntax

    type tensor = A.t
    type t = (tensor list * Q.t) list

    let multiply (t1, c1) (t2, c2) =
      (t1 @ t2, Q.mul c1 c2)

    let compress terms =
      List.map (fun (t, cs) -> (t, Q.sum cs)) (ThoList.factorize terms)

    let rec of_expr e =
      compress (of_expr' e)

    and of_expr' = function
      | S.Integer i -> [([], Q.make i 1)]
      | S.Float _ -> invalid_arg "UFOx.Tensor.of_expr: unexpected float"
      | S.Variable name ->
	 invalid_arg ("UFOx.Tensor.of_expr: unexpected variable '" ^
			 name ^ "'")
      | S.Application (name, args) -> [([A.of_expr name args], Q.unit)]
      | S.Sum terms -> ThoList.flatmap of_expr terms
      | S.Difference (e1, e2) ->
	 of_expr (S.Sum [e1; S.Product [S.Integer (-1); e2]])
      | S.Product factors ->
	 List.fold_right
	   (fun e acc -> Product.list2 multiply (of_expr e) acc)
	   factors [([], Q.unit)]
      | S.Quotient (n, d) ->
	 begin match of_expr d with
	 | [([], q)] ->
	    List.map (fun (t, c) -> (t, Q.div c q)) (of_expr n)
	 | [] ->
	    failwith "UFOx.Tensor.of_expr: zero denominator"
	 | _ ->
	    failwith "UFOx.Tensor.of_expr: only integer denominators allowed"
	 end
      | S.Power (e, p) ->
	 begin match of_expr e, of_expr p with
	 | [([], q)], [([], p)] ->
	    if Q.is_integer p then
	      [([], Q.pow q (Q.to_integer p))]
	    else
	      failwith "UFOx.Tensor.of_expr: rational power"
	 | [([], q)], _ ->
	    failwith "UFOx.Tensor.of_expr: non-numeric power"
	 | _ -> failwith "UFOx.Tensor.of_expr: power of tensor"
	 end
	 
    let term_to_string (tensors, c) =
      if Q.is_null c then
	""
      else
	(if Q.is_negative c then " - " else " + ") ^
	  (let c = Q.abs c in
	   if Q.is_unit c && tensors = [] then
	     ""
	   else
	     Q.to_string c) ^
	  (match tensors with
	  | [] -> ""
	  | tensors ->
	     (if Q.is_unit (Q.abs c) then "" else "*") ^
	       String.concat "*" (List.map A.to_string tensors))

    let term_to_string (tensors, c) =
      if Q.is_null c then
	""
      else
	(if Q.is_negative c then " - " else " + ") ^
	  (let c = Q.abs c in
	   match tensors with
	   | [] -> Q.to_string c
	   | tensors ->
	      String.concat "*"
		((if Q.is_unit c then [] else [Q.to_string c]) @
		    List.map A.to_string tensors))

    let to_string terms =
      String.concat "" (List.map term_to_string terms)
      
  end

module Atomic_Lorentz =
  struct
	
    type t =
      | C of int * int
      | Epsilon of int * int * int * int
      | Gamma of int * int * int
      | Gamma5 of int * int
      | Identity of int * int
      | Metric of int * int
      | P of int * int
      | ProjP of int * int
      | ProjM of int * int
      | Sigma of int * int * int * int

    let to_string = function
      | C (i, j) ->
	 Printf.sprintf "C(%d,%d)" i j
      | Epsilon (mu, nu, ka, la) ->
	 Printf.sprintf "Epsilon(%d,%d,%d,%d)" mu nu ka la
      | Gamma (mu, i, j) ->
	 Printf.sprintf "Gamma(%d,%d,%d)" mu i j
      | Gamma5 (i, j) ->
	 Printf.sprintf "Gamma5(%d,%d)" i j
      | Identity (i, j) ->
	 Printf.sprintf "Identity(%d,%d)" i j
      | Metric (mu, nu) ->
	 Printf.sprintf "Metric(%d,%d)" mu nu
      | P (mu, n) ->
	 Printf.sprintf "P(%d,%d)" mu n
      | ProjP (i, j) ->
	 Printf.sprintf "ProjP(%d,%d)" i j
      | ProjM (i, j) ->
	 Printf.sprintf "ProjM(%d,%d)" i j
      | Sigma (mu, nu, i, j) ->
	 Printf.sprintf "Sigma(%d,%d,%d,%d)" mu nu i j

    module S = UFOx_syntax

    let of_expr name args =
      match name, args with
      | "C", [S.Integer i; S.Integer j] -> C (i, j)
      | "C", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to C()"
      | "Epsilon", [S.Integer mu; S.Integer nu; S.Integer ka; S.Integer la] ->
	 Epsilon (mu, nu, ka, la)
      | "Epsilon", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Epsilon()"
      | "Gamma", [S.Integer mu; S.Integer i; S.Integer j] ->
	 Gamma (mu, i, j)
      | "Gamma", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma()"
      | "Gamma5", [S.Integer i; S.Integer j] -> Gamma5 (i, j)
      | "Gamma5", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma5()"
      | "Identity", [S.Integer i; S.Integer j] -> Identity (i, j)
      | "Identity", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Identity()"
      | "Metric", [S.Integer mu; S.Integer nu] -> Metric (mu, nu)
      | "Metric", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Metric()"
      | "P", [S.Integer mu; S.Integer n] -> P (mu, n)
      | "P", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to P()"
      | "ProjP", [S.Integer i; S.Integer j] -> ProjP (i, j)
      | "ProjP", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjP()"
      | "ProjM", [S.Integer i; S.Integer j] -> ProjM (i, j)
      | "ProjM", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjM()"
      | "Sigma", [S.Integer mu; S.Integer nu; S.Integer i; S.Integer j] ->
	 Sigma (mu, nu, i, j)
      | "Sigma", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Sigma()"
      | name, _ ->
	 invalid_arg ("UFOx.Lorentz.of_expr: invalid tensor '" ^ name ^ "'")

    type index_classes =
      { vector : int list;
	spinor : int list;
	conj_spinor : int list }

    let index_classes vector conj_spinor spinor =
      { vector = positive vector;
	conj_spinor = positive conj_spinor;
	spinor = positive spinor }

    let classify_indices1 = function
      | C (i, j) -> index_classes [] [i] [j] (* ??? *)
      | Gamma5 (i, j) | Identity (i, j)
      | ProjP (i, j) | ProjM (i, j) -> index_classes [] [i] [j]
      | Epsilon (mu, nu, ka, la) -> index_classes [mu; nu; ka; la] [] []
      | Gamma (mu, i, j) -> index_classes [mu] [i] [j]
      | Metric (mu, nu) -> index_classes [mu; nu] [] []
      | P (mu, _) -> index_classes [mu] [] []
      | Sigma (mu, nu, i, j) -> index_classes [mu; nu] [i] [j]

    let classify_indices tensors =
      List.fold_right
	(fun v acc ->
	  let i = classify_indices1 v in
	  { vector = i.vector @ acc.vector;
	    spinor = i.spinor @ acc.spinor;
	    conj_spinor = i.conj_spinor @ acc.conj_spinor })
	tensors { vector = []; conj_spinor = []; spinor = [] }

  end
    
module Lorentz =
  struct

    module L = Tensor(Atomic_Lorentz)
    type t = L.t
      
    let of_expr = L.of_expr
    let to_string = L.to_string

  end

module Atomic_Color =
  struct

    type t =
      | Identity of int * int
      | T of int * int * int
      | F of int * int * int
      | D of int * int * int
      | Epsilon of int * int * int
      | EpsilonBar of int * int * int
      | T6 of int * int * int
      | K6 of int * int * int
      | K6Bar of int * int * int

    module S = UFOx_syntax

    let of_expr name args =
      match name, args with
      | "Identity", [S.Integer i; S.Integer j] -> Identity (i, j)
      | "Identity", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to Identity()"
      | "T", [S.Integer a; S.Integer i; S.Integer j] -> T (a, i, j)
      | "T", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to T()"
      | "f", [S.Integer a; S.Integer b; S.Integer c] -> F (a, b, c)
      | "f", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to f()"
      | "d", [S.Integer a; S.Integer b; S.Integer c] -> D (a, b, c)
      | "d", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to d()"
      | "Epsilon", [S.Integer i; S.Integer j; S.Integer k] ->
	 Epsilon (i, j, k)
      | "Epsilon", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to Epsilon()"
      | "EpsilonBar", [S.Integer i; S.Integer j; S.Integer k] ->
	 EpsilonBar (i, j, k)
      | "EpsilonBar", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to EpsilonBar()"
      | "T6", [S.Integer a; S.Integer i'; S.Integer j'] -> T6 (a, i', j')
      | "T6", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to T6()"
      | "K6", [S.Integer i'; S.Integer j; S.Integer k] -> K6 (i', j, k)
      | "K6", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to K6()"
      | "K6Bar", [S.Integer i'; S.Integer j; S.Integer k] -> K6Bar (i', j, k)
      | "K6Bar", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to K6Bar()"
      | name, _ ->
	 invalid_arg ("UFOx.Color.of_expr: invalid tensor '" ^ name ^ "'")
	
    let to_string = function
      | Identity (i, j) -> Printf.sprintf "Identity(%d,%d)" i j
      | T (a, i, j) -> Printf.sprintf "T(%d,%d,%d)" a i j
      | F (a, b, c) -> Printf.sprintf "f(%d,%d,%d)" a b c
      | D (a, b, c) -> Printf.sprintf "d(%d,%d,%d)" a b c
      | Epsilon (i, j, k) -> Printf.sprintf "Epsilon(%d,%d,%d)" i j k
      | EpsilonBar (i, j, k) -> Printf.sprintf "EpsilonBar(%d,%d,%d)" i j k
      | T6 (a, i', j') -> Printf.sprintf "T6(%d,%d,%d)" a i' j'
      | K6 (i', j, k) -> Printf.sprintf "K6(%d,%d,%d)" i' j k
      | K6Bar (i', j, k) -> Printf.sprintf "K6Bar(%d,%d,%d)" i' j k

    type index_classes =
      { fundamental : int list;
	conjugate : int list;
	adjoint : int list }

    let index_classes fundamental conjugate adjoint =
      { fundamental = positive fundamental;
	conjugate = positive conjugate;
	adjoint = positive adjoint }

    let classify_indices1 = function
      | Identity (i, j) -> index_classes [i] [j] []
      | T (a, i, j) -> index_classes [i] [j] [a]
      | F (a, b, c) | D (a, b, c) -> index_classes [] [] [a; b; c]
      | Epsilon (i, j, k) -> index_classes [i; j; k] [] []
      | EpsilonBar (i, j, k) -> index_classes [] [i; j; k] []
      | T6 (a, i', j') ->
	 failwith "UFOx.Color: sextets not supported yet!"
      | K6 (i', j, k) ->
	 failwith "UFOx.Color: sextets not supported yet!"
      | K6Bar (i', j, k) ->
	 failwith "UFOx.Color: sextets not supported yet!"

    let classify_indices tensors =
      List.fold_right
	(fun v acc ->
	  let i = classify_indices1 v in
	  { fundamental = i.fundamental @ acc.fundamental;
	    conjugate = i.conjugate @ acc.conjugate;
	    adjoint = i.adjoint @ acc.adjoint })
	tensors { fundamental = []; conjugate = []; adjoint = [] }

  end

module Color =
  struct

    module C = Tensor(Atomic_Color)
    type t = C.t
      
    let of_expr = C.of_expr
    let to_string = C.to_string

  end

module Value =
  struct
  end

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

