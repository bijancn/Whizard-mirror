(* $Id: options.ml 4926 2013-12-04 12:35:06Z jr_reuter $

   Copyright (C) 1999-2014 by

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

module A = Map.Make (struct type t = string let compare = compare end)

type t =
    { actions : Arg.spec A.t;
      raw : (string * Arg.spec * string) list }

let empty = { actions = A.empty; raw = [] }

let extend old options =
  { actions = List.fold_left
      (fun a (s, f, _) -> A.add s f a) old.actions options;
    raw = options @ old.raw }

let create = extend empty

let merge o1 o2 =
  extend o1 o2.raw

exception Invalid of string * string

let parse options (name, value) =
  try
    match A.find name options.actions with
    | Arg.Unit f -> f ()
    | Arg.Set b -> b := true
    | Arg.Clear b -> b := false
    | Arg.String f -> f value
    | Arg.Int f -> f (int_of_string value)
    | Arg.Float f -> f (float_of_string value)
    | _ -> invalid_arg "Options.parse"
  with
  | Not_found -> raise (Invalid (name, value))

let list options =
  List.map (fun (o, _, d) -> (o, d)) options.raw

let cmdline prefix options =
  List.map (fun (o, f, d) -> (prefix ^ o, f, d)) options.raw
  

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
