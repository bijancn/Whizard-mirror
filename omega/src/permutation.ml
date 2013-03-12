(* $Id: permutation.ml 4015 2013-01-03 16:04:18Z jr_reuter $

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

module type T =
  sig
    type t
    val of_list : int list -> t
    val of_array : int array -> t
    val inverse : t -> t
    val compose : t -> t -> t
    val list : t -> 'a list -> 'a list
    val array : t -> 'a array -> 'a array
  end

module Using_Lists : T =
  struct

    type t = int list

    let of_list p =
      if List.sort compare p <> (ThoList.range 0 (List.length p - 1)) then
	invalid_arg "Permutation.of_list"
      else
	p

    let of_array p =
      try
	of_list (Array.to_list p)
      with 
      | Invalid_argument "Permutation.of_list" ->
	invalid_arg "Permutation.of_array"

    let inverse p = snd (ThoList.ariadne_sort p)

    let list p l =
      List.map snd
	(List.sort compare
	   (try
	      List.rev_map2 (fun i x -> (i, x)) p l
	    with
	    | Invalid_argument "List.rev_map2" ->
	      invalid_arg "Permutation.list: length mismatch"))

    let array p a =
      try
	Array.of_list (list p (Array.to_list a))
      with 
      | Invalid_argument "Permutation.list: length mismatch" ->
	invalid_arg "Permutation.array: length mismatch"

(* Probably not optimal (or really inefficient), but correct by
   associativity. *)

    let compose p q =
      list (inverse q) p

  end

module Using_Arrays : T =
  struct

    type t = int array

    let of_list p =
      if List.sort compare p <> (ThoList.range 0 (List.length p - 1)) then
	invalid_arg "Permutation.of_list"
      else
	Array.of_list p

    let of_array p =
      try
	of_list (Array.to_list p)
      with 
      | Invalid_argument "Permutation.of_list" ->
	invalid_arg "Permutation.of_array"
      
      let inverse p =
      let len_p = Array.length p in
      let p' = Array.make len_p p.(0) in
      for i = 0 to pred len_p do
	p'.(p.(i)) <- i
      done;
      p'

    let array p a =
      let len_a = Array.length a
      and len_p = Array.length p in
      if len_a <> len_p then
	invalid_arg "Permutation.array: length mismatch";
      let a' = Array.make len_a a.(0) in
      for i = 0 to pred len_a do
	a'.(p.(i)) <- a.(i)
      done;
      a'

    let list p l =
      try
	Array.to_list (array p (Array.of_list l))
      with 
      | Invalid_argument "Permutation.array: length mismatch" ->
	invalid_arg "Permutation.list: length mismatch"

    let compose p q =
      array (inverse q) p

  end

module Default = Using_Arrays

