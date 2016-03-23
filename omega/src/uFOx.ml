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

    type t = (int * int * tensor list) list

    module S = UFOx_syntax

    let of_expr = function
      | S.Integer i -> (i, 1, [])
      | S.Application ("C", [S.Integer i; S.Integer j]) ->
	 (1, 1, [C (i, j)])
      | S.Application ("Epsilon",
		       [S.Integer mu; S.Integer nu;
			S.Integer ka; S.Integer la]) ->
	 (1, 1, [Epsilon (mu, nu, ka, la)])

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

