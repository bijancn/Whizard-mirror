(* $Id: options.mli 7444 2016-02-17 15:37:20Z jr_reuter $

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

type t

val empty : t
val create : (string * Arg.spec * string) list -> t

val extend : t -> (string * Arg.spec * string) list -> t
(*i val merge : t -> t -> t i*)

val cmdline : string -> t -> (string * Arg.spec * string) list

(*i val list : t -> (string * string) list i*)
(*i val parse : t -> string * string -> unit i*)
(*i exception Invalid of string * string i*)

(* This is a clone of [Arg.parse] with a delayed usage string. *)
val parse : (string * Arg.spec * string) list ->
  (string -> unit) -> (unit -> string) -> unit

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
