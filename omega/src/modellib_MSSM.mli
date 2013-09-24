(* $Id: modellib_MSSM.mli 4538 2013-08-23 16:09:06Z jr_reuter $

   Copyright (C) 1999-2013 by

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

(* \thocwmodulesection{More Hardcoded Models} *)

module type MSSM_flags = 
  sig 
    val include_goldstone : bool 
    val include_four      : bool
    val ckm_present       : bool
    val gravitino         : bool
    val higgs_triangle    : bool
  end

module MSSM_no_goldstone : MSSM_flags
module MSSM_goldstone : MSSM_flags
module MSSM_no_4 : MSSM_flags
module MSSM_no_4_ckm : MSSM_flags
module MSSM_Grav : MSSM_flags
module MSSM_Hgg : MSSM_flags
module MSSM : functor (F: MSSM_flags) -> Model.T with module Ch = Charges.QQ

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
