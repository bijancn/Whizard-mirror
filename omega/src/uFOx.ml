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

(* \thocwmodulesubsection{Naive Rational Arithmetic} *)

(* \begin{dubious}
     This \emph{is} dangerous and will overflow even for simple
     applications.  The production code will have to be linked to
     a library for large integer arithmetic.
   \end{dubious} *)

(* Anyway, here's Euclid's algorithm: *)
let rec gcd i1 i2 =
  if i2 = 0 then
    abs i1
  else
    gcd i2 (i1 mod i2)

let lcm i1 i2 = (i1 / gcd i1 i2) * i2

module Q =
  struct
    type t = int * int
    let is_null (n, _) = (n = 0)
    let is_unit (n, d) = (n <> 0) && (n = d)
    let null = (0, 1)
    let unit = (1, 1)
    let make n d =
      let c = gcd n d in
      (n / c, d / c)
    let mul (n1, d1) (n2, d2) = make (n1 * n2) (d1 * d2)
    let add (n1, d1) (n2, d2) = make (n1 * d2 + n2 * d1) (d1 * d2)
    let sub (n1, d1) (n2, d2) = make (n1 * d2 - n2 * d1) (d1 * d2)
    let neg (n, d) = (- n, d)
    let to_ratio (n, d) =
      if d < 0 then
        (-n, -d)
      else
        (n, d)
    let to_float (n, d) = float n /. float d
    let to_string (n, d) =
      if d = 1 then
        Printf.sprintf "%d" n
      else
        Printf.sprintf "(%d/%d)" n d
  end

module Lorentz =
  struct

    type tensor =
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

    let tensor_to_string = function
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

    let term_to_string (n, d, tensors) =
      if n = 0 then
	""
      else
	(if n < 0 then " - " else " + ") ^
	  (let n = abs n in
	   if n = 1 && d = 1 then
	     ""
	   else
	     string_of_int n ^
	       (if d = 1 then "" else "/" ^ string_of_int d) ^ "*") ^
	  String.concat "*" (List.map tensor_to_string tensors)

    let term_to_string' (n, d, tensors) =
      string_of_int n ^ "/" ^ string_of_int d ^ "*" ^
	String.concat "*" (List.map tensor_to_string tensors)

    type t = (int * int * tensor list) list

    let to_string terms =
      String.concat "" (List.map term_to_string terms)
      
    let multiply (n1, d1, t1) (n2, d2, t2) =
      let n', d' = Q.mul (n1, d1) (n2, d2) in
      (n', d', t1 @ t2)

    module S = UFOx_syntax

    let rec of_expr = function
      | S.Integer i ->
	 [(i, 1, [])]
      | S.Float _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: unexpected float"
      | S.Variable name ->
	 invalid_arg ("UFOx.Lorentz.of_expr: unexpected variable '" ^ name ^ "'")
      | S.Application ("C", [S.Integer i; S.Integer j]) ->
	 [(1, 1, [C (i, j)])]
      | S.Application ("C", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to C()"
      | S.Application ("Epsilon",
		       [S.Integer mu; S.Integer nu;
			S.Integer ka; S.Integer la]) ->
	 [(1, 1, [Epsilon (mu, nu, ka, la)])]
      | S.Application ("Epsilon", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Epsilon()"
      | S.Application ("Gamma",
		       [S.Integer mu; S.Integer i; S.Integer j]) ->
	 [(1, 1, [Gamma (mu, i, j)])]
      | S.Application ("Gamma", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma()"
      | S.Application ("Gamma5", [S.Integer i; S.Integer j]) ->
	 [(1, 1, [Gamma5 (i, j)])]
      | S.Application ("Gamma5", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma5()"
      | S.Application ("Identity", [S.Integer i; S.Integer j]) ->
	 [(1, 1, [Identity (i, j)])]
      | S.Application ("Identity", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Identity()"
      | S.Application ("Metric", [S.Integer mu; S.Integer nu]) ->
	 [(1, 1, [Metric (mu, nu)])]
      | S.Application ("Metric", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Metric()"
      | S.Application ("P", [S.Integer mu; S.Integer n]) ->
	 [(1, 1, [P (mu, n)])]
      | S.Application ("P", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to P()"
      | S.Application ("ProjP", [S.Integer i; S.Integer j]) ->
	 [(1, 1, [ProjP (i, j)])]
      | S.Application ("ProjP", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjP()"
      | S.Application ("ProjM", [S.Integer i; S.Integer j]) ->
	 [(1, 1, [ProjM (i, j)])]
      | S.Application ("ProjM", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjM()"
      | S.Application ("Sigma",
		       [S.Integer mu; S.Integer nu;
			S.Integer i; S.Integer j]) ->
	 [(1, 1, [Sigma (mu, nu, i, j)])]
      | S.Application ("Sigma", _) ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Sigma()"
      | S.Application (name, _) ->
	 invalid_arg ("UFOx.Lorentz.of_expr: invalid tensor '" ^ name ^ "'")
      | S.Sum terms ->
	 ThoList.flatmap of_expr terms
      | S.Difference (_, _) ->
	 failwith "UFOx.Lorentz.of_expr"
      | S.Product factors ->
	 List.fold_right
	   (fun e acc -> Product.list2 multiply (of_expr e) acc)
	   factors [(1, 1, [])]
      | S.Quotient (n, d) ->
	 let d' = of_expr d in
	 failwith "UFOx.Lorentz.of_expr"
      | S.Power (_, _) ->
	 failwith "UFOx.Lorentz.of_expr"
	 
    type index_types =
      { vector : int list;
	spinor : int list;
	conj_spinor : int list }

    let index_types vector conj_spinor spinor =
      { vector = positive vector;
	conj_spinor = positive conj_spinor;
	spinor = positive spinor }

    let classify_indices = function
      | C (i, j) -> index_types [] [i] [j] (* ??? *)
      | Gamma5 (i, j) | Identity (i, j)
      | ProjP (i, j) | ProjM (i, j) -> index_types [] [i] [j]
      | Epsilon (mu, nu, ka, la) -> index_types [mu; nu; ka; la] [] []
      | Gamma (mu, i, j) -> index_types [mu] [i] [j]
      | Metric (mu, nu) -> index_types [mu; nu] [] []
      | P (mu, _) -> index_types [mu] [] []
      | Sigma (mu, nu, i, j) -> index_types [mu; nu] [i] [j]

    let classify_indices_list tensors =
      List.fold_right
	(fun v acc ->
	  let i = classify_indices v in
	  { vector = i.vector @ acc.vector;
	    spinor = i.spinor @ acc.spinor;
	    conj_spinor = i.conj_spinor @ acc.conj_spinor })
	tensors { vector = []; conj_spinor = []; spinor = [] }

  end

module Color =
  struct

    type tensor =
      | Unit
      | Identity of int * int
      | T of int * int * int
      | F of int * int * int
      | D of int * int * int
      | Epsilon of int * int * int
      | EpsilonBar of int * int * int
      | T6 of int * int * int
      | K6 of int * int * int
      | K6Bar of int * int * int

    type index_types =
      { fundamental : int list;
	conjugate : int list;
	adjoint : int list }

    let index_types fundamental conjugate adjoint =
      { fundamental = positive fundamental;
	conjugate = positive conjugate;
	adjoint = positive adjoint }

    let classify_indices = function
      | Unit -> index_types [] [] []
      | Identity (i, j) -> index_types [i] [j] []
      | T (a, i, j) -> index_types [i] [j] [a]
      | F (a, b, c) | D (a, b, c) -> index_types [] [] [a; b; c]
      | Epsilon (i, j, k) -> index_types [i; j; k] [] []
      | EpsilonBar (i, j, k) -> index_types [] [i; j; k] []
      | T6 (a, i', j') ->
	 failwith "UFOx.Color: sextets not supported yet!"
      | K6 (i', j, k) ->
	 failwith "UFOx.Color: sextets not supported yet!"
      | K6Bar (i', j, k) ->
	 failwith "UFOx.Color: sextets not supported yet!"

    let classify_indices_list tensors =
      List.fold_right
	(fun v acc ->
	  let i = classify_indices v in
	  { fundamental = i.fundamental @ acc.fundamental;
	    conjugate = i.conjugate @ acc.conjugate;
	    adjoint = i.adjoint @ acc.adjoint })
	tensors { fundamental = []; conjugate = []; adjoint = [] }

  end

module Value =
  struct
  end

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

