(* $Id: colorize.ml 6943 2015-05-01 10:53:21Z msekulla $

   Copyright (C) 1999-2015 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
       Christian Speckner <cnspeckn@googlemail.com>
       Marco Sekulla <sekulla@physik.uni-siegen.de>

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

let rcs_file = RCS.parse "Colorize" ["Colorizing Monochrome Models"]
    { RCS.revision = "$Revision: 6943 $";
      RCS.date = "$Date: 2015-05-01 12:53:21 +0200 (Fri, 01 May 2015) $";
      RCS.author = "$Author: msekulla $";
      RCS.source
        = "$URL: svn+ssh://bchokoufe@svn.hepforge.org/hepforge/svn/whizard/trunk/omega/src/colorize.ml $" }

(* \thocwmodulesection{Colorizing a Monochrome Model} *)

module It (M : Model.T) = 
  struct

    let rcs = RCS.rename rcs_file "Colorize.It()"
        [ "Colorizing Generic Monochrome Models"]

    open Coupling

    module C = Color

    let incomplete s =
      failwith ("Colorize.It()." ^ s ^ " not done yet!")

    let invalid s =
      invalid_arg ("Colorize.It()." ^ s ^ " must not be evaluated!")

    let impossible s =
      invalid_arg ("Colorize.It()." ^ s ^ " can't happen! (but just did ...)")

    let su0 s =
      invalid_arg ("Colorize.It()." ^ s ^ ": found SU(0)!")

    let colored_vertex s =
      invalid_arg ("Colorize.It()." ^ s ^ ": colored vertex!")

    let baryonic_vertex s =
      invalid_arg ("Colorize.It()." ^ s ^
                   ": baryonic (i.e. eps_ijk) vertices not supported yet!")

    let color_flow_ambiguous s =
      invalid_arg ("Colorize.It()." ^ s ^ ": ambiguous color flow!")

    let color_flow_of_string s =
      let c = int_of_string s in
      if c < 1 then
        invalid_arg ("Colorize.It()." ^ s ^ ": color flow # < 1!")
      else
        c
        
    type cf_in = int
    type cf_out = int

    type flavor =
      | White of M.flavor
      | CF_in of M.flavor * cf_in
      | CF_out of M.flavor * cf_out
      | CF_io of M.flavor * cf_in * cf_out
      | CF_aux of M.flavor

    type flavor_sans_color = M.flavor

    let flavor_sans_color = function
      | White f -> f
      | CF_in (f, _) -> f
      | CF_out (f, _) -> f
      | CF_io (f, _, _) -> f
      | CF_aux f -> f

    let pullback f arg1 =
      f (flavor_sans_color arg1)

    type gauge = M.gauge
    type constant = M.constant
    let options = M.options

    let color = pullback M.color
    let pdg = pullback M.pdg
    let lorentz = pullback M.lorentz

    module Ch = M.Ch
    let charges = pullback M.charges

(* For the propagator we cannot use pullback because we have to add the case
   of the color singlet propagator by hand. *)

    let cf_aux_propagator = function
      | Prop_Scalar -> Prop_Col_Scalar  (* Spin 0 octets. *)
      | Prop_Majorana -> Prop_Col_Majorana   (* Spin 1/2 octets. *)
      | Prop_Feynman -> Prop_Col_Feynman   (* Spin 1 states, massless. *)
      | Prop_Unitarity -> Prop_Col_Unitarity   (* Spin 1 states, massive. *)
      | Aux_Scalar -> Aux_Col_Scalar  (* constant colored scalar propagator *)
      | Aux_Vector -> Aux_Col_Vector  (* constant colored vector propagator *)
      | Aux_Tensor_1 -> Aux_Col_Tensor_1  (* constant colored tensor propagator *)
      | Prop_Col_Scalar | Prop_Col_Feynman
      | Prop_Col_Majorana | Prop_Col_Unitarity
      | Aux_Col_Scalar | Aux_Col_Vector | Aux_Col_Tensor_1
        -> failwith ("Colorize.It().colorize_propagator: already colored particle!")
      | _ -> failwith ("Colorize.It().colorize_propagator: impossible!")

    let propagator = function
      | CF_aux f -> cf_aux_propagator (M.propagator f)
      | White f -> M.propagator f
      | CF_in (f, _) -> M.propagator f
      | CF_out (f, _) -> M.propagator f
      | CF_io (f, _, _) -> M.propagator f

    let width = pullback M.width

    let goldstone = function
      | White f ->
          begin match M.goldstone f with
          | None -> None
          | Some (f', g) -> Some (White f', g)
          end
      | CF_in (f, c) ->
          begin match M.goldstone f with
          | None -> None
          | Some (f', g) -> Some (CF_in (f', c), g)
          end
      | CF_out (f, c) ->
          begin match M.goldstone f with
          | None -> None
          | Some (f', g) -> Some (CF_out (f', c), g)
          end
      | CF_io (f, c1, c2) ->
          begin match M.goldstone f with
          | None -> None
          | Some (f', g) -> Some (CF_io (f', c1, c2), g)
          end
      | CF_aux f ->
          begin match M.goldstone f with
          | None -> None
          | Some (f', g) -> Some (CF_aux f', g)
          end

    let conjugate = function
      | White f -> White (M.conjugate f)
      | CF_in (f, c) -> CF_out (M.conjugate f, c)
      | CF_out (f, c) -> CF_in (M.conjugate f, c)
      | CF_io (f, c1, c2) -> CF_io (M.conjugate f, c2, c1)
      | CF_aux f -> CF_aux (M.conjugate f)

    let conjugate_sans_color = M.conjugate

    let fermion = pullback M.fermion

    let max_degree = M.max_degree

    let flavors () =
      invalid "flavors"

    let external_flavors () =
      invalid "external_flavors"

    let parameters = M.parameters

    module ISet = Set.Make (struct type t = int let compare = compare end)

    let nc_value =
      let nc_set =
        List.fold_left
          (fun nc_set f ->
            match M.color f with
            | C.Singlet -> nc_set
            | C.SUN nc -> ISet.add (abs nc) nc_set
            | C.AdjSUN nc -> ISet.add (abs nc) nc_set)
          ISet.empty (M.flavors ()) in
      match ISet.elements nc_set with
      | [] -> 0
      | [n] -> n
      | nc_list -> 
          invalid_arg
            ("Colorize.It(): more than one value of N_C: " ^
             String.concat ", " (List.map string_of_int nc_list))

    let nc () =
      nc_value

    let split_color_string s =
      try
        let i1 = String.index s '/' in
        let i2 = String.index_from s (succ i1) '/' in
        let sf = String.sub s 0 i1
        and sc1 = String.sub s (succ i1) (i2 - i1 - 1)
        and sc2 = String.sub s (succ i2) (String.length s - i2 - 1) in
        (sf, sc1, sc2)
      with
      | Not_found -> (s, "", "")

    let flavor_of_string s =
      try 
        let sf, sc1, sc2 = split_color_string s in
        let f = M.flavor_of_string sf in
        match M.color f with
        | C.Singlet -> White f
        | C.SUN nc ->
            if nc > 0 then
              CF_in (f, color_flow_of_string sc1)
            else
              CF_out (f, color_flow_of_string sc2)
        | C.AdjSUN _ ->
            begin match sc1, sc2 with
            | "", "" -> CF_aux f
            | _, _ -> CF_io (f, color_flow_of_string sc1, color_flow_of_string sc2)
            end
      with
      | Failure "int_of_string" ->
          invalid_arg "Colorize().flavor_of_string: expecting integer"

    let flavor_to_string = function
      | White f ->
          M.flavor_to_string f
      | CF_in (f, c) ->
          M.flavor_to_string f ^ "/" ^ string_of_int c ^ "/"
      | CF_out (f, c) ->
          M.flavor_to_string f ^ "//" ^ string_of_int c
      | CF_io (f, c1, c2) ->
          M.flavor_to_string f ^ "/" ^ string_of_int c1 ^ "/" ^ string_of_int c2
      | CF_aux f ->
          M.flavor_to_string f ^ "//"

    let flavor_to_TeX = function
      | White f ->
          M.flavor_to_TeX f
      | CF_in (f, c) ->
          "{" ^ M.flavor_to_TeX f ^ "}_{\\mathstrut " ^ string_of_int c ^ "}"
      | CF_out (f, c) ->
          "{" ^ M.flavor_to_TeX f ^ "}_{\\mathstrut\\overline{" ^
          string_of_int c ^ "}}"
      | CF_io (f, c1, c2) ->
          "{" ^ M.flavor_to_TeX f ^ "}_{\\mathstrut " ^
          string_of_int c1 ^ "\\overline{" ^ string_of_int c2 ^ "}}"
      | CF_aux f ->
          "{" ^ M.flavor_to_TeX f ^ "}_{\\mathstrut 0}"

    let flavor_symbol = function
      | White f ->
          M.flavor_symbol f
      | CF_in (f, c) ->
          M.flavor_symbol f ^ "_" ^ string_of_int c ^ "_"
      | CF_out (f, c) ->
          M.flavor_symbol f ^ "__" ^ string_of_int c
      | CF_io (f, c1, c2) ->
          M.flavor_symbol f ^ "_" ^ string_of_int c1 ^ "_" ^ string_of_int c2
      | CF_aux f ->
          M.flavor_symbol f ^ "__"

    let gauge_symbol = M.gauge_symbol

(* Masses and widths must not depend on the colors anyway! *)
    let mass_symbol = pullback M.mass_symbol
    let width_symbol = pullback M.width_symbol

    let constant_symbol = M.constant_symbol

(* \thocwmodulesubsection{Vertices} *)

(* \thocwmodulesubsection{Auxiliary functions} *)

    let mult_vertex3 x = function
      | FBF (c, fb, coup, f) ->
          FBF ((x * c), fb, coup, f) 
      | PBP (c, fb, coup, f) ->
          PBP ((x * c), fb, coup, f) 
      | BBB (c, fb, coup, f) ->
          BBB ((x * c), fb, coup, f) 
      | GBG (c, fb, coup, f) ->
          GBG ((x * c), fb, coup, f) 
      | Gauge_Gauge_Gauge c ->
          Gauge_Gauge_Gauge (x * c)
      | Aux_Gauge_Gauge c ->
          Aux_Gauge_Gauge (x * c)
      | Scalar_Vector_Vector c ->
          Scalar_Vector_Vector (x * c)
      | Aux_Vector_Vector c ->
          Aux_Vector_Vector (x * c)
      | Aux_Scalar_Vector c ->
          Aux_Scalar_Vector (x * c) 
      | Scalar_Scalar_Scalar c ->
          Scalar_Scalar_Scalar (x * c)
      | Aux_Scalar_Scalar c ->
          Aux_Scalar_Scalar (x * c) 
      | Vector_Scalar_Scalar c ->
          Vector_Scalar_Scalar (x * c) 
      | Graviton_Scalar_Scalar c ->
          Graviton_Scalar_Scalar (x * c)
      | Graviton_Vector_Vector c ->
          Graviton_Vector_Vector (x * c)
      | Graviton_Spinor_Spinor c ->
          Graviton_Spinor_Spinor (x * c) 
      | Dim4_Vector_Vector_Vector_T c ->
          Dim4_Vector_Vector_Vector_T (x * c)
      | Dim4_Vector_Vector_Vector_L c ->
          Dim4_Vector_Vector_Vector_L (x * c)
      | Dim4_Vector_Vector_Vector_T5 c ->
          Dim4_Vector_Vector_Vector_T5 (x * c) 
      | Dim4_Vector_Vector_Vector_L5 c ->
          Dim4_Vector_Vector_Vector_L5 (x * c)
      | Dim6_Gauge_Gauge_Gauge c ->
          Dim6_Gauge_Gauge_Gauge (x * c)
      | Dim6_Gauge_Gauge_Gauge_5 c ->
          Dim6_Gauge_Gauge_Gauge_5 (x * c)
      | Aux_DScalar_DScalar c ->
          Aux_DScalar_DScalar (x * c)
      | Aux_Vector_DScalar c ->
          Aux_Vector_DScalar (x * c)
      | Dim5_Scalar_Gauge2 c ->
          Dim5_Scalar_Gauge2 (x * c) 
      | Dim5_Scalar_Gauge2_Skew c ->
          Dim5_Scalar_Gauge2_Skew (x * c)
      | Dim5_Scalar_Vector_Vector_T c ->
          Dim5_Scalar_Vector_Vector_T (x * c)
      | Dim5_Scalar_Vector_Vector_U c ->
          Dim5_Scalar_Vector_Vector_U (x * c)
      | Dim5_Scalar_Vector_Vector_TU c ->
          Dim5_Scalar_Vector_Vector_TU (x * c)
      | Dim5_Scalar_Scalar2 c ->
          Dim5_Scalar_Scalar2 (x * c)
      | Scalar_Vector_Vector_t c ->
          Scalar_Vector_Vector_t (x * c)
      | Dim6_Vector_Vector_Vector_T c ->
          Dim6_Vector_Vector_Vector_T (x * c)
      | Tensor_2_Vector_Vector c ->
          Tensor_2_Vector_Vector (x * c)
      | Tensor_2_Vector_Vector_cf c ->
          Tensor_2_Vector_Vector_cf (x * c)
      | Tensor_2_Scalar_Scalar c ->
          Tensor_2_Scalar_Scalar (x * c)
      | Tensor_2_Scalar_Scalar_cf c ->
          Tensor_2_Scalar_Scalar_cf (x * c)
      | Tensor_2_Vector_Vector_1 c ->
          Tensor_2_Vector_Vector_1 (x * c)
      | Tensor_2_Vector_Vector_t c ->
          Tensor_2_Vector_Vector_t (x * c)
      | Dim5_Tensor_2_Vector_Vector_1 c ->
          Dim5_Tensor_2_Vector_Vector_1 (x * c)
      | Dim5_Tensor_2_Vector_Vector_2 c ->
          Dim5_Tensor_2_Vector_Vector_2 (x * c)
      | TensorVector_Vector_Vector c ->
          TensorVector_Vector_Vector (x * c)
      | TensorVector_Vector_Vector_cf c ->
          TensorVector_Vector_Vector_cf (x * c)
      | TensorVector_Scalar_Scalar c ->
          TensorVector_Scalar_Scalar (x * c)
      | TensorVector_Scalar_Scalar_cf c ->
          TensorVector_Scalar_Scalar_cf (x * c)
      | TensorScalar_Vector_Vector c ->
          TensorScalar_Vector_Vector (x * c)
      | TensorScalar_Vector_Vector_cf c ->
          TensorScalar_Vector_Vector_cf (x * c)
      | TensorScalar_Scalar_Scalar c ->
          TensorScalar_Scalar_Scalar (x * c)
      | TensorScalar_Scalar_Scalar_cf c ->
          TensorScalar_Scalar_Scalar_cf (x * c)
      | Dim7_Tensor_2_Vector_Vector_T c ->
          Dim7_Tensor_2_Vector_Vector_T (x * c)

    let mult_vertex4 x = function
      | Scalar4 c ->
          Scalar4 (x * c) 
      | Scalar2_Vector2 c ->
          Scalar2_Vector2 (x * c)
      | Vector4 ic4_list ->
          Vector4 (List.map (fun (c, icl) -> (x * c, icl)) ic4_list)
      | DScalar4 ic4_list ->
          DScalar4 (List.map (fun (c, icl) -> (x * c, icl)) ic4_list)
      | DScalar2_Vector2 ic4_list ->
          DScalar2_Vector2 (List.map (fun (c, icl) -> (x * c, icl)) ic4_list)
      | GBBG (c, fb, b2, f) ->
          GBBG ((x * c), fb, b2, f)
      | Vector4_K_Matrix_tho (c, ic4_list) ->
          Vector4_K_Matrix_tho ((x * c),  ic4_list)
      | Vector4_K_Matrix_jr (c, ch2_list) ->
          Vector4_K_Matrix_jr ((x * c),  ch2_list)
      | DScalar2_Vector2_K_Matrix_ms (c, ch2_list) ->
          DScalar2_Vector2_K_Matrix_ms ((x * c),  ch2_list)
      | DScalar4_K_Matrix_ms (c, ch2_list) ->
          DScalar4_K_Matrix_ms ((x * c),  ch2_list)
      | Dim8_Scalar2_Vector2_1 c ->
          Dim8_Scalar2_Vector2_1 (x * c) 
      | Dim8_Scalar2_Vector2_2 c ->
          Dim8_Scalar2_Vector2_1 (x * c)
      | Dim8_Scalar4 c ->
          Dim8_Scalar4 (x * c)

    let mult_vertexn x = function
      | foo -> ignore (incomplete "mult_vertexn"); foo

    let mult_vertex x = function
      | V3 (v, fuse, c) -> V3 (mult_vertex3 x v, fuse, c)
      | V4 (v, fuse, c) -> V4 (mult_vertex4 x v, fuse, c)
      | Vn (v, fuse, c) -> Vn (mult_vertexn x v, fuse, c)

(* Below, we will need to permute Lorentz structures.  The following
   permutes the three possible contractions of four vectors.  We permute
   the first three indices, as they correspond to the particles entering
   the fusion. *)

    type permutation4 =
      | P123 | P231 | P312
      | P213 | P321 | P132

    let permute_contract4 = function
      | P123 ->
          begin function
            | C_12_34 -> C_12_34
            | C_13_42 -> C_13_42
            | C_14_23 -> C_14_23
          end
      | P231 ->
          begin function
            | C_12_34 -> C_14_23
            | C_13_42 -> C_12_34
            | C_14_23 -> C_13_42
          end
      | P312 ->
          begin function
            | C_12_34 -> C_13_42
            | C_13_42 -> C_14_23
            | C_14_23 -> C_12_34
          end
      | P213 ->
          begin function
            | C_12_34 -> C_12_34
            | C_13_42 -> C_14_23
            | C_14_23 -> C_13_42
          end
      | P321 ->
          begin function
            | C_12_34 -> C_14_23
            | C_13_42 -> C_13_42
            | C_14_23 -> C_12_34
          end
      | P132 ->
          begin function
            | C_12_34 -> C_13_42
            | C_13_42 -> C_12_34
            | C_14_23 -> C_14_23
          end

    let permute_contract4_list perm ic4_list =
      List.map (fun (i, c4) -> (i, permute_contract4 perm c4)) ic4_list

    let permute_vertex4' perm = function
      | Scalar4 c ->
          Scalar4 c
      | Vector4 ic4_list ->
          Vector4 (permute_contract4_list perm ic4_list)
      | Vector4_K_Matrix_jr (c, ic4_list) ->
          Vector4_K_Matrix_jr (c, permute_contract4_list perm ic4_list)
      | DScalar2_Vector2_K_Matrix_ms (c, ic4_list) ->
          DScalar2_Vector2_K_Matrix_ms (c, permute_contract4_list perm ic4_list)
      | DScalar4_K_Matrix_ms (c, ic4_list) ->
          DScalar4_K_Matrix_ms (c, permute_contract4_list perm ic4_list)
      | Scalar2_Vector2 c ->
          incomplete "permute_vertex4' Scalar2_Vector2"
      | DScalar4 ic4_list ->
          incomplete "permute_vertex4' DScalar4"
      | DScalar2_Vector2 ic4_list ->
          incomplete "permute_vertex4' DScalar2_Vector2"
      | GBBG (c, fb, b2, f) ->
          incomplete "permute_vertex4' GBBG"
      | Vector4_K_Matrix_tho (c, ch2_list) ->
          incomplete "permute_vertex4' Vector4_K_Matrix_tho"

    let permute_vertex4 perm = function
      | V3 (v, fuse, c) -> V3 (v, fuse, c)
      | V4 (v, fuse, c) -> V4 (permute_vertex4' perm v, fuse, c)
      | Vn (v, fuse, c) -> Vn (v, fuse, c)

(* [vertices] are \emph{only} used by functor applications and
   for indexing a cache of precomputed fusion rules, which is not
   used for colorized models. *)

    let vertices () =
      invalid "vertices"

(* \thocwmodulesubsection{Cubic Vertices} *)

(* \begin{dubious}
     The following pattern matches could eventually become quite long.
     The O'Caml compiler will (hopefully) optimize them aggressively
     (\url{http://pauillac.inria.fr/~maranget/papers/opat/}).
   \end{dubious} *)

    let colorize_fusion2 f1 f2 (f, v) =
      match M.color f with

      | C.Singlet ->
          begin match f1, f2 with

          | White _, White _ ->
              [White f, v]

          | CF_in (_, c1), CF_out (_, c2')
          | CF_out (_, c1), CF_in (_, c2') ->
              if c1 = c2' then
                [White f, v]
              else
                []

          | CF_io (f1, c1, c1'), CF_io (f2, c2, c2') ->
              if c1 = c2' && c2 = c1' then
                [White f, v]
              else
                []

          | CF_aux f1, CF_aux f2 ->
              [White f, mult_vertex (- (nc ())) v]

          | CF_aux _, CF_io _ | CF_io _, CF_aux _ ->
              []

          | (CF_in _ | CF_out _ | CF_io _ | CF_aux _), White _
          | White _, (CF_in _ | CF_out _ | CF_io _ | CF_aux _)
          | (CF_io _ | CF_aux _), (CF_in _ | CF_out _)
          | (CF_in _ | CF_out _), (CF_io _ | CF_aux _)
          | CF_in _, CF_in _ | CF_out _, CF_out _ ->
              colored_vertex "colorize_fusion2"

          end

      | C.SUN nc1 ->
          begin match f1, f2 with

          | CF_in (_, c1), (White _ | CF_aux _)
          | (White _ | CF_aux _), CF_in (_, c1) ->
              if nc1 > 0 then
                [CF_in (f, c1), v]
              else
                colored_vertex "colorize_fusion2"

          | CF_out (_, c1'), (White _ | CF_aux _)
          | (White _ | CF_aux _), CF_out (_, c1') ->
              if nc1 < 0 then
                [CF_out (f, c1'), v]
              else
                colored_vertex "colorize_fusion2"

          | CF_in (_, c1), CF_io (_, c2, c2')
          | CF_io (_, c2, c2'), CF_in (_, c1) ->
              if nc1 > 0 then begin
                if c1 = c2' then
                  [CF_in (f, c2), v]
                else
                  []
              end else
                colored_vertex "colorize_fusion2"

          | CF_out (_, c1'), CF_io (_, c2, c2')
          | CF_io (_, c2, c2'), CF_out (_, c1') ->
              if nc1 < 0 then begin
                if c1' = c2 then
                  [CF_out (f, c2'), v]
                else
                  []
              end else
                colored_vertex "colorize_fusion2"

          | CF_in _, CF_in _ ->
              if nc1 > 0 then
                baryonic_vertex "colorize_fusion2"
              else
                colored_vertex "colorize_fusion2"

          | CF_out _, CF_out _ ->
              if nc1 < 0 then
                baryonic_vertex "colorize_fusion2"
              else
                colored_vertex "colorize_fusion2"

          | CF_in _, CF_out _ | CF_out _, CF_in _
          | (White _ | CF_io _ | CF_aux _),
                (White _ | CF_io _ | CF_aux _) ->
              colored_vertex "colorize_fusion2"

          end

      | C.AdjSUN _ ->
          begin match f1, f2 with

          | White _, CF_io (_, c1, c2') | CF_io (_, c1, c2'), White _ ->
              [CF_io (f, c1, c2'), v]

          | White _, CF_aux _ | CF_aux _, White _ ->
              [CF_aux f, mult_vertex (- (nc ())) v]

          | CF_in (_, c1), CF_out (_, c2')
          | CF_out (_, c2'), CF_in (_, c1) ->
              if c1 <> c2' then
                [CF_io (f, c1, c2'), v]
              else
                [CF_aux f, v]

(* In the adjoint representation
   \begin{subequations}
   \begin{equation}
     \parbox{28mm}{\fmfframe(2,2)(2,1){\begin{fmfgraph*}(24,24)
       \fmfsurround{d1,e1,d2,e2,d3,e3}
       \fmf{gluon}{v,e1}
       \fmf{gluon}{v,e2}
       \fmf{gluon}{v,e3}
       \fmflabel{1}{e1}
       \fmflabel{2}{e2}
       \fmflabel{3}{e3}
       \fmfdot{v}
       \fmffreeze
       \fmf{warrow_right}{v,e1}
       \fmf{warrow_right}{v,e2}
       \fmf{warrow_right}{v,e3}
     \end{fmfgraph*}}} \,= 
     %begin{split}
       g f_{a_1a_2a_3} C^{\mu_1\mu_2\mu_3} (k_1,k_2,k_3)
     %end{split}
   \end{equation}
   with
   \begin{multline}
   \label{eq:C123}
     C^{\mu_1\mu_2\mu_3}(k_1,k_2,k_3) = \\
             (   g^{\mu_1\mu_2} (k_1^{\mu_3}-k_2^{\mu_3})
               + g^{\mu_2\mu_3} (k_2^{\mu_1}-k_3^{\mu_1})
               + g^{\mu_3\mu_1} (k_3^{\mu_2}-k_1^{\mu_2}) )
   \end{multline}
   \end{subequations}
   while in the color flow basis find from
   \begin{equation}
     \ii f_{a_1a_2a_3}
       = \tr\left(T_{a_1}\left\lbrack T_{a_2},T_{a_3}\right\rbrack\right)
       = \tr\left(T_{a_1}T_{a_2}T_{a_3}\right)
       - \tr\left(T_{a_1}T_{a_3}T_{a_2}\right)
   \end{equation}
   the decomposition
   \begin{equation}
       \ii f_{a_1a_2a_3} T_{a_1}^{i_1j_1}T_{a_2}^{i_2j_2}T_{a_3}^{i_3j_3}
     = \delta^{i_1j_2}\delta^{i_2j_3}\delta^{i_3j_1}
     - \delta^{i_1j_3}\delta^{i_3j_2}\delta^{i_2j_1}\,.
   \end{equation}
   The resulting Feynman rule is
   \begin{equation}
     \parbox{28mm}{\fmfframe(2,2)(2,1){\begin{fmfgraph*}(24,24)
       \fmfsurround{d1,e1,d2,e2,d3,e3}
       \fmf{phantom}{v,e1}
       \fmf{phantom}{v,e2}
       \fmf{phantom}{v,e3}
       \fmflabel{1}{e1}
       \fmflabel{2}{e2}
       \fmflabel{3}{e3}
       \fmffreeze
       \fmfi{phantom_arrow}{(reverse vpath (__e1, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e2, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(reverse vpath (__e2, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e3, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(reverse vpath (__e3, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e1, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e1, __v) sideways -thick)
         join (        vpath (__e2, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e2, __v) sideways -thick)
         join (        vpath (__e3, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e3, __v) sideways -thick)
         join (        vpath (__e1, __v) sideways -thick)}
     \end{fmfgraph*}}} \,= 
         \ii g
         \left(   \delta^{i_1j_3}\delta^{i_2j_1}\delta^{i_3j_2} 
                - \delta^{i_1j_2}\delta^{i_2j_3}\delta^{i_3j_1} \right)
         C^{\mu_1\mu_2\mu_3} (k_1,k_2,k_3)
   \end{equation} *)

(* \begin{dubious}
     We have to generalize this for cases of three particles
     in the adjoint that are not all gluons (gluinos, scalar octets):
     \begin{itemize}
       \item scalar-scalar-scalar
       \item scalar-scalar-vector
       \item scalar-vector-vector
       \item scalar-fermion-fermion
       \item vector-fermion-fermion
     \end{itemize}
   \end{dubious} *)

(* \begin{dubious}
     We could use a better understanding of the signs for the
     gaugino-gaugino-gaugeboson couplings!!! 
   \end{dubious} *)

          | CF_io (f1, c1, c1'), CF_io (f2, c2, c2') ->
              let sign =
                begin match v with
                | V3 (Gauge_Gauge_Gauge _, _, _)
                | V3 (Aux_Gauge_Gauge _, _, _) -> 1
                | V3 (FBF (_, _, _, _), fuse2, _) ->
                    begin match fuse2 with
                    | F12 ->  1 (* works, but needs theoretical underpinning *)
                    | F21 -> -1 (* dto. *)
                    | F31 ->  1 (* dto. *)
                    | F32 -> -1 (* transposition of [F12] (no testcase) *)
                    | F23 ->  1 (* transposition of [F21] (no testcase) *)
                    | F13 -> -1 (* transposition of [F12] (no testcase) *)
                    end
                | V3 _ -> incomplete "colorize_fusion2 (V3 _)"
                | V4 _ -> impossible "colorize_fusion2 (V4 _)"
                | Vn _ -> impossible "colorize_fusion2 (Vn _)"
                end in
              if c1' = c2 then
                [CF_io (f, c1, c2'), mult_vertex (-sign) v]
              else if c2' = c1 then
                [CF_io (f, c2, c1'), mult_vertex ( sign) v]
              else
                []

          | CF_aux _ , CF_io _
          | CF_io _ , CF_aux _
          | CF_aux _ , CF_aux _ ->
              []

          | White _, White _
          | (White _ | CF_io _ | CF_aux _), (CF_in _ | CF_out _)
          | (CF_in _ | CF_out _), (White _ | CF_io _ | CF_aux _)
          | CF_in _, CF_in _ | CF_out _, CF_out _ -> 
              colored_vertex "colorize_fusion2"

          end

(* \thocwmodulesubsection{Quartic Vertices} *)

    let colorize_fusion3 f1 f2 f3 (f, v) =
      match M.color f with

      | C.Singlet ->
          begin match f1, f2, f3 with

          | White _, White _, White _ ->
              [White f, v]

          | (White _ | CF_aux _), CF_in (_, c1), CF_out (_, c2')
          | (White _ | CF_aux _), CF_out (_, c1), CF_in (_, c2')
          | CF_in (_, c1), (White _ | CF_aux _), CF_out (_, c2')
          | CF_out (_, c1), (White _ | CF_aux _), CF_in (_, c2')
          | CF_in (_, c1), CF_out (_, c2'), (White _ | CF_aux _)
          | CF_out (_, c1), CF_in (_, c2'), (White _ | CF_aux _) ->
              if c1 = c2' then
                [White f, v]
              else
                []

          | White _, CF_io (_, c1, c1'), CF_io (_, c2, c2')
          | CF_io (_, c1, c1'), White _, CF_io (_, c2, c2')
          | CF_io (_, c1, c1'), CF_io (_, c2, c2'), White _ ->
              if c1 = c2' && c2 = c1' then
                [White f, v]
              else
                []

          | White _, CF_aux _, CF_aux _
          | CF_aux _, White _, CF_aux _
          | CF_aux _, CF_aux _, White _ ->
              [White f, mult_vertex (- (nc ())) v]

          | White _, CF_io _, CF_aux _
          | White _, CF_aux _, CF_io _
          | CF_io _, White _, CF_aux _
          | CF_aux _, White _, CF_io _
          | CF_io _, CF_aux _, White _
          | CF_aux _, CF_io _, White _ ->
              []

          | CF_io (_, c1, c1'), CF_in (_, c2), CF_out (_, c3')
          | CF_io (_, c1, c1'), CF_out (_, c3'), CF_in (_, c2)
          | CF_in (_, c2), CF_io (_, c1, c1'), CF_out (_, c3')
          | CF_out (_, c3'), CF_io (_, c1, c1'), CF_in (_, c2)
          | CF_in (_, c2), CF_out (_, c3'), CF_io (_, c1, c1')
          | CF_out (_, c3'), CF_in (_, c2), CF_io (_, c1, c1') ->
              if c1 = c3' && c1' = c2 then
                [White f, v]
              else
                []

          | CF_io (_, c1, c1'), CF_io (_, c2, c2'), CF_io (_, c3, c3') ->
              if c1' = c2 && c2' = c3 && c3' = c1 then
                [White f, mult_vertex (-1) v]
              else if c1' = c3 && c2' = c1 && c3' = c2 then
                [White f, mult_vertex ( 1) v]
              else
                []

          | CF_io _, CF_io _, CF_aux _
          | CF_io _, CF_aux _, CF_io _
          | CF_aux _, CF_io _, CF_io _
          | CF_io _, CF_aux _, CF_aux _
          | CF_aux _, CF_io _, CF_aux _
          | CF_aux _, CF_aux _, CF_io _
          | CF_aux _, CF_aux _, CF_aux _ ->
              []
         
          | CF_in _, CF_in _, CF_in _
          | CF_out _, CF_out _, CF_out _ ->
              baryonic_vertex "colorize_fusion3"

          | CF_in _, CF_in _, CF_out _
          | CF_in _, CF_out _, CF_in _
          | CF_out _, CF_in _, CF_in _
          | CF_in _, CF_out _, CF_out _
          | CF_out _, CF_in _, CF_out _
          | CF_out _, CF_out _, CF_in _ 

          | White _, White _, (CF_io _ | CF_aux _)
          | White _, (CF_io _ | CF_aux _), White _
          | (CF_io _ | CF_aux _), White _, White _

          | (White _ | CF_io _ | CF_aux _), CF_in _, CF_in _
          | CF_in _, (White _ | CF_io _ | CF_aux _), CF_in _
          | CF_in _, CF_in _, (White _ | CF_io _ | CF_aux _)

          | (White _ | CF_io _ | CF_aux _), CF_out _, CF_out _
          | CF_out _, (White _ | CF_io _ | CF_aux _), CF_out _
          | CF_out _, CF_out _, (White _ | CF_io _ | CF_aux _)

          | (CF_in _ | CF_out _),
              (White _ | CF_io _ | CF_aux _),
              (White _ | CF_io _ | CF_aux _)
          | (White _ | CF_io _ | CF_aux _),
              (CF_in _ | CF_out _),
              (White _ | CF_io _ | CF_aux _)
          | (White _ | CF_io _ | CF_aux _),
              (White _ | CF_io _ | CF_aux _),
              (CF_in _ | CF_out _) ->
                colored_vertex "colorize_fusion3"

          end

      | C.SUN nc1 ->
          begin match f1, f2, f3 with

          | CF_in (_, c1), CF_io (_, c2, c2'), CF_io (_, c3, c3')
          | CF_io (_, c2, c2'), CF_in (_, c1), CF_io (_, c3, c3')
          | CF_io (_, c2, c2'), CF_io (_, c3, c3'), CF_in (_, c1) ->
              if nc1 > 0 then
                if c1 = c2' && c2 = c3' then
                  [CF_in (f, c3), v]
                else if c1 = c3' && c3 = c2' then
                  [CF_in (f, c2), v]
                else
                  []
              else
                colored_vertex "colorize_fusion3"

          | CF_out (_, c1'), CF_io (_, c2, c2'), CF_io (_, c3, c3')
          | CF_io (_, c2, c2'), CF_out (_, c1'), CF_io (_, c3, c3')
          | CF_io (_, c2, c2'), CF_io (_, c3, c3'), CF_out (_, c1') ->
              if nc1 < 0 then
                if c1' = c2 && c2' = c3 then
                  [CF_out (f, c3'), v]
                else if c1' = c3 && c3' = c2 then
                  [CF_out (f, c2'), v]
                else
                  []
              else
                colored_vertex "colorize_fusion3"

          | CF_aux _, CF_in (_, c1), CF_io (_, c2, c2')
          | CF_aux _, CF_io (_, c2, c2'), CF_in (_, c1)
          | CF_in (_, c1), CF_aux _, CF_io (_, c2, c2')
          | CF_io (_, c2, c2'), CF_aux _, CF_in (_, c1)
          | CF_in (_, c1), CF_io (_, c2, c2'), CF_aux _
          | CF_io (_, c2, c2'), CF_in (_, c1), CF_aux _ ->
              if nc1 > 0 then
                if c1 = c2' then
                  [CF_in (f, c2), mult_vertex ( 2) v]
                else
                  []
              else
                colored_vertex "colorize_fusion3"

          | CF_aux _, CF_out (_, c1'), CF_io (_, c2, c2')
          | CF_aux _, CF_io (_, c2, c2'), CF_out (_, c1')
          | CF_out (_, c1'), CF_aux _, CF_io (_, c2, c2')
          | CF_io (_, c2, c2'), CF_aux _, CF_out (_, c1')
          | CF_out (_, c1'), CF_io (_, c2, c2'), CF_aux _
          | CF_io (_, c2, c2'), CF_out (_, c1'), CF_aux _ ->
              if nc1 < 0 then
                if c1' = c2 then
                  [CF_out (f, c2'), mult_vertex ( 2) v]
                else
                  []
              else
                colored_vertex "colorize_fusion3"

          | White _, CF_in (_, c1), CF_io (_, c2, c2')
          | White _, CF_io (_, c2, c2'), CF_in (_, c1)
          | CF_in (_, c1), White _, CF_io (_, c2, c2')
          | CF_io (_, c2, c2'), White _, CF_in (_, c1)
          | CF_in (_, c1), CF_io (_, c2, c2'), White _
          | CF_io (_, c2, c2'), CF_in (_, c1), White _ ->
              if nc1 > 0 then
                if c1 = c2' then
                  [CF_in (f, c2), v]
                else
                  []
              else
                colored_vertex "colorize_fusion3"

          | White _, CF_out (_, c1'), CF_io (_, c2, c2')
          | White _, CF_io (_, c2, c2'), CF_out (_, c1')
          | CF_out (_, c1'), White _, CF_io (_, c2, c2')
          | CF_io (_, c2, c2'), White _, CF_out (_, c1')
          | CF_out (_, c1'), CF_io (_, c2, c2'), White _
          | CF_io (_, c2, c2'), CF_out (_, c1'), White _ ->
              if nc1 < 0 then
                if c2 = c1' then
                  [CF_out (f, c2'), v]
                else
                  []
              else
                colored_vertex "colorize_fusion3"

          | CF_in (_, c1), CF_aux _, CF_aux _
          | CF_aux _, CF_in (_, c1), CF_aux _
          | CF_aux _, CF_aux _, CF_in (_, c1) ->
              if nc1 > 0 then
                [CF_in (f, c1), mult_vertex ( 2) v]
              else
                colored_vertex "colorize_fusion3"

          | CF_in (_, c1), CF_aux _, White _
          | CF_in (_, c1), White _, CF_aux _
          | CF_in (_, c1), White _, White _
          | CF_aux _, CF_in (_, c1), White _
          | White _, CF_in (_, c1), CF_aux _
          | White _, CF_in (_, c1), White _
          | CF_aux _, White _, CF_in (_, c1)
          | White _, CF_aux _, CF_in (_, c1)
          | White _, White _, CF_in (_, c1) ->
              if nc1 > 0 then
                [CF_in (f, c1), v]
              else
                colored_vertex "colorize_fusion3"

          | CF_out (_, c1'), CF_aux _, CF_aux _
          | CF_aux _, CF_out (_, c1'), CF_aux _
          | CF_aux _, CF_aux _, CF_out (_, c1') ->
              if nc1 < 0 then
                [CF_out (f, c1'), mult_vertex ( 2) v]
              else
                colored_vertex "colorize_fusion3"

          | CF_out (_, c1'), CF_aux _, White _
          | CF_out (_, c1'), White _, CF_aux _
          | CF_out (_, c1'), White _, White _
          | CF_aux _, CF_out (_, c1'), White _
          | White _, CF_out (_, c1'), CF_aux _
          | White _, CF_out (_, c1'), White _
          | CF_aux _, White _, CF_out (_, c1')
          | White _, CF_aux _, CF_out (_, c1')
          | White _, White _, CF_out (_, c1') ->
              if nc1 < 0 then
                [CF_out (f, c1'), v]
              else
                colored_vertex "colorize_fusion3"

          | CF_in _, CF_in _, CF_out _
          | CF_in _, CF_out _, CF_in _
          | CF_out _, CF_in _, CF_in _ ->
              if nc1 > 0 then
                color_flow_ambiguous "colorize_fusion3"
              else
                colored_vertex "colorize_fusion3"

          | CF_in _, CF_out _, CF_out _
          | CF_out _, CF_in _, CF_out _ 
          | CF_out _, CF_out _, CF_in _ ->
              if nc1 < 0 then
                color_flow_ambiguous "colorize_fusion3"
              else
                colored_vertex "colorize_fusion3"

          | CF_in _, CF_in _, CF_in _
          | CF_out _, CF_out _, CF_out _

          | (White _ | CF_io _ | CF_aux _),
            (White _ | CF_io _ | CF_aux _),
            (White _ | CF_io _ | CF_aux _)

          | (CF_in _ | CF_out _),
              (CF_in _ | CF_out _),
              (White _ | CF_io _ | CF_aux _)
          | (CF_in _ | CF_out _),
              (White _ | CF_io _ | CF_aux _),
              (CF_in _ | CF_out _)
          | (White _ | CF_io _ | CF_aux _),
              (CF_in _ | CF_out _),
              (CF_in _ | CF_out _) ->
              colored_vertex "colorize_fusion3"

          end

      | C.AdjSUN nc ->
          begin match f1, f2, f3 with

          | CF_in (_, c1), CF_out (_, c1'), White _
          | CF_out (_, c1'), CF_in (_, c1), White _
          | CF_in (_, c1), White _, CF_out (_, c1')
          | CF_out (_, c1'), White _, CF_in (_, c1)
          | White _, CF_in (_, c1), CF_out (_, c1')
          | White _, CF_out (_, c1'), CF_in (_, c1) ->
              if c1 <> c1' then
                [CF_io (f, c1, c1'), v]
              else
                [CF_aux f, v]

          | CF_in (_, c1), CF_out (_, c1'), CF_aux _
          | CF_out (_, c1'), CF_in (_, c1), CF_aux _
          | CF_in (_, c1), CF_aux _, CF_out (_, c1')
          | CF_out (_, c1'), CF_aux _, CF_in (_, c1)
          | CF_aux _, CF_in (_, c1), CF_out (_, c1')
          | CF_aux _, CF_out (_, c1'), CF_in (_, c1) ->
              if c1 <> c1' then
                [CF_io (f, c1, c1'), mult_vertex ( 2) v]
              else
                [CF_aux f, mult_vertex ( 2) v]

          | CF_in (_, c1), CF_out (_, c1'), CF_io (_, c2, c2')
          | CF_out (_, c1'), CF_in (_, c1), CF_io (_, c2, c2')
          | CF_in (_, c1), CF_io (_, c2, c2'), CF_out (_, c1')
          | CF_out (_, c1'), CF_io (_, c2, c2'), CF_in (_, c1)
          | CF_io (_, c2, c2'), CF_in (_, c1), CF_out (_, c1')
          | CF_io (_, c2, c2'), CF_out (_, c1'), CF_in (_, c1) ->
              if c1 = c2' && c2 = c1' then
                [CF_aux f, mult_vertex ( 2) v]
              else if c1 = c2' then
                [CF_io (f, c2, c1'), v]
              else if c2 = c1' then
                [CF_io (f, c1, c2'), v]
              else
                []

(* \begin{equation}
     \parbox{28mm}{\fmfframe(2,2)(2,1){\begin{fmfgraph*}(24,24)
       \fmfsurround{d1,e1,d2,e2,d3,e3,d4,e4}
       \fmf{gluon}{v,e1}
       \fmf{gluon}{v,e2}
       \fmf{gluon}{v,e3}
       \fmf{gluon}{v,e4}
       \fmflabel{1}{e1}
       \fmflabel{2}{e2}
       \fmflabel{3}{e3}
       \fmflabel{4}{e4}
       \fmfdot{v}
       \fmffreeze
       \fmf{warrow_right}{v,e1}
       \fmf{warrow_right}{v,e2}
       \fmf{warrow_right}{v,e3}
       \fmf{warrow_right}{v,e4}
     \end{fmfgraph*}}} \,= 
     \begin{split}
         \mbox{} - & \ii g^2 f_{a_1a_2b}f_{a_3a_4b}
                     (g_{\mu_1\mu_3} g_{\mu_4\mu_2} - g_{\mu_1\mu_4} g_{\mu_2\mu_3}) \\
         \mbox{} - & \ii g^2 f_{a_1a_3b}f_{a_4a_2b}
                     (g_{\mu_1\mu_4} g_{\mu_2\mu_3} - g_{\mu_1\mu_2} g_{\mu_3\mu_4}) \\
         \mbox{} - & \ii g^2 f_{a_1a_4b}f_{a_2a_3b}
                     (g_{\mu_1\mu_2} g_{\mu_3\mu_4} - g_{\mu_1\mu_3} g_{\mu_4\mu_2})
     \end{split}
   \end{equation} *)

(* Using
   \begin{equation}
     \mathcal{P}_4 = \left\{\{1,2,3,4\},\{1,3,4,2\},\{1,4,2,3\},
                            \{1,2,4,3\},\{1,4,3,2\},\{1,3,2,4\}\right\}
   \end{equation}
   as the set of permutations of~$\{1,2,3,4\}$ with the cyclic permutations
   factored out, we have:
   \begin{equation}
     \parbox{28mm}{\fmfframe(2,2)(2,1){\begin{fmfgraph*}(24,24)
       \fmfsurround{d1,e1,d2,e2,d3,e3,d4,e4}
       \fmf{phantom}{v,e1}
       \fmf{phantom}{v,e2}
       \fmf{phantom}{v,e3}
       \fmf{phantom}{v,e4}
       \fmflabel{1}{e1}
       \fmflabel{2}{e2}
       \fmflabel{3}{e3}
       \fmflabel{4}{e4}
       \fmffreeze
       \fmfi{phantom_arrow}{(reverse vpath (__e1, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e2, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(reverse vpath (__e2, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e3, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(reverse vpath (__e3, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e4, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(reverse vpath (__e4, __v) sideways -thick)}
       \fmfi{phantom_arrow}{(        vpath (__e1, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e1, __v) sideways -thick)
         join (        vpath (__e2, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e2, __v) sideways -thick)
         join (        vpath (__e3, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e3, __v) sideways -thick)
         join (        vpath (__e4, __v) sideways -thick)}
       \fmfi{plain}{%
              (reverse vpath (__e4, __v) sideways -thick)
         join (        vpath (__e1, __v) sideways -thick)}
     \end{fmfgraph*}}} \,=
     \begin{aligned}
      \ii g^2 \sum_{\{\alpha_k\}_{k=1,2,3,4}\in\mathcal{P}_4}
         \delta^{i_{\alpha_1}j_{\alpha_2}}\delta^{i_{\alpha_2}j_{\alpha_3}}
         \delta^{i_{\alpha_3}j_{\alpha_4}}\delta^{i_{\alpha_4}j_{\alpha_1}}\qquad\qquad\\
             \left(   2g_{\mu_{\alpha_1}\mu_{\alpha_3}} g_{\mu_{\alpha_4}\mu_{\alpha_2}}
                    -  g_{\mu_{\alpha_1}\mu_{\alpha_4}} g_{\mu_{\alpha_2}\mu_{\alpha_3}}
                    -  g_{\mu_{\alpha_1}\mu_{\alpha_2}} g_{\mu_{\alpha_3}\mu_{\alpha_4}}\right) 
     \end{aligned}
   \end{equation} *)

(* The different color connections correspond to permutations of the
   particles entering the fusion and have to be matched by a corresponding
   permutation of the Lorentz structure: *)

(* \begin{dubious}
     We have to generalize this for cases of four particles
     in the adjoint that are not all gluons:
     \begin{itemize}
       \item scalar-scalar-scalar-scalar
       \item scalar-scalar-vector-vector
     \end{itemize}
     and even ones including fermions (gluinos) if higher dimensional
     operators are involved.
   \end{dubious} *)

          | CF_io (_, c1, c1'), CF_io (_, c2, c2'), CF_io (_, c3, c3') ->
              if      c1' = c2 && c2' = c3 then
                [CF_io (f, c1, c3'), permute_vertex4 P123 v]
              else if c1' = c3 && c3' = c2 then
                [CF_io (f, c1, c2'), permute_vertex4 P132 v]
              else if c2' = c3 && c3' = c1 then
                [CF_io (f, c2, c1'), permute_vertex4 P231 v]
              else if c2' = c1 && c1' = c3 then
                [CF_io (f, c2, c3'), permute_vertex4 P213 v]
              else if c3' = c1 && c1' = c2 then
                [CF_io (f, c3, c2'), permute_vertex4 P312 v]
              else if c3' = c2 && c2' = c1 then
                [CF_io (f, c3, c1'), permute_vertex4 P321 v]
              else
                []
          | CF_io _, CF_io _, CF_aux _
          | CF_io _, CF_aux _, CF_io _
          | CF_aux _, CF_io _, CF_io _
          | CF_io _, CF_aux _, CF_aux _
          | CF_aux _, CF_aux _, CF_io _
          | CF_aux _, CF_io _, CF_aux _
          | CF_aux _, CF_aux _, CF_aux _ ->
              []

          | CF_io (_, c1, c1'), CF_io (_, c2, c2'), White _
          | CF_io (_, c1, c1'), White _, CF_io (_, c2, c2')
          | White _, CF_io (_, c1, c1'), CF_io (_, c2, c2') ->
              if c1' = c2 then
                [CF_io (f, c1, c2'), mult_vertex (-1) v]
              else if c2' = c1 then
                [CF_io (f, c2, c1'), mult_vertex ( 1) v]
              else
                []

          | CF_io (_, c1, c1'), CF_aux _, White _
          | CF_aux _, CF_io (_, c1, c1'), White _
          | CF_io (_, c1, c1'), White _, CF_aux _
          | CF_aux _, White _, CF_io (_, c1, c1')
          | White _, CF_io (_, c1, c1'), CF_aux _
          | White _, CF_aux _, CF_io (_, c1, c1') ->
              []

          | CF_aux _, CF_aux _, White _
          | CF_aux _, White _, CF_aux _
          | White _, CF_aux _, CF_aux _ ->
              []

          | White _, White _, CF_io (_, c1, c1')
          | White _, CF_io (_, c1, c1'), White _
          | CF_io (_, c1, c1'), White _, White _ ->
              [CF_io (f, c1, c1'), v]

          | White _, White _, CF_aux _
          | White _, CF_aux _, White _
          | CF_aux _, White _, White _ ->
              []

          | White _, White _, White _

          | (White _ | CF_io _ | CF_aux _),
              (White _ | CF_io _ | CF_aux _),
              (CF_in _ | CF_out _)
          | (White _ | CF_io _ | CF_aux _),
              (CF_in _ | CF_out _),
              (White _ | CF_io _ | CF_aux _)
          | (CF_in _ | CF_out _),
              (White _ | CF_io _ | CF_aux _),
              (White _ | CF_io _ | CF_aux _)

          | CF_in _, CF_in _, (White _ | CF_io _ | CF_aux _)
          | CF_in _, (White _ | CF_io _ | CF_aux _), CF_in _
          | (White _ | CF_io _ | CF_aux _), CF_in _, CF_in _

          | CF_out _, CF_out _, (White _ | CF_io _ | CF_aux _)
          | CF_out _, (White _ | CF_io _ | CF_aux _), CF_out _
          | (White _ | CF_io _ | CF_aux _), CF_out _, CF_out _

          | (CF_in _ | CF_out _),
              (CF_in _ | CF_out _),
              (CF_in _ | CF_out _) ->
              colored_vertex "colorize_fusion3"

          end

(* \thocwmodulesubsection{Quintic and Higher Vertices} *)

    let is_white = function
      | White _ -> true
      | _ -> false

    let colorize_fusionn flist (f, v) =
      let incomplete_match () =
        incomplete
          ("colorize_fusionn { " ^
           String.concat ", " (List.map flavor_to_string flist) ^
           " } -> " ^ M.flavor_to_string f) in
      match M.color f with
      | C.Singlet ->
          if List.for_all is_white flist then
            [White f, v]
          else
            incomplete_match ()
      | C.SUN _ ->
          if List.for_all is_white flist then
            colored_vertex "colorize_fusionn"
          else
            incomplete_match ()
      | C.AdjSUN _ ->
          if List.for_all is_white flist then
            colored_vertex "colorize_fusionn"
          else
            incomplete_match ()

    let fuse2 f1 f2 =
      ThoList.flatmap
        (colorize_fusion2 f1 f2)
        (M.fuse2 (flavor_sans_color f1) (flavor_sans_color f2))

    let fuse3 f1 f2 f3 =
      ThoList.flatmap
        (colorize_fusion3 f1 f2 f3)
        (M.fuse3 (flavor_sans_color f1) (flavor_sans_color f2) (flavor_sans_color f3))

    let fuse_list flist =
      ThoList.flatmap
        (colorize_fusionn flist)
        (M.fuse (List.map flavor_sans_color flist))

    let fuse = function
      | [] | [_] -> invalid_arg "Colorize.It().fuse"
      | [f1; f2] -> fuse2 f1 f2
      | [f1; f2; f3] -> fuse3 f1 f2 f3
      | flist -> fuse_list flist
            
    let max_degree = M.max_degree

(* \thocwmodulesubsection{Adding Color to External Particles} *)

    let count_color_strings f_list =
      let rec count_color_strings' n_in n_out n_glue = function
        | f :: rest ->
            begin match M.color f with
            | C.Singlet -> count_color_strings' n_in n_out n_glue rest
            | C.SUN nc ->
                if nc > 0 then
                  count_color_strings' (succ n_in) n_out n_glue rest
                else if nc < 0 then
                  count_color_strings' n_in (succ n_out) n_glue rest
                else
                  su0 "count_color_strings"
            | C.AdjSUN _ ->
                count_color_strings' (succ n_in) (succ n_out) (succ n_glue) rest
            end
        | [] -> (n_in, n_out, n_glue)
      in
      count_color_strings' 0 0 0 f_list

    let external_color_flows f_list =
      let n_in, n_out, n_glue = count_color_strings f_list in
      if n_in <> n_out then
        []
      else
        let color_strings = ThoList.range 1 n_in in
        List.rev_map
          (fun permutation -> (color_strings, permutation))
          (Combinatorics.permute color_strings)

(* If there are only adjoints \emph{and} there are no couplings of
   adjoints to singlets, we can ignore the $\mathrm{U}(1)$-ghosts. *)

    let pure_adjoints f_list =
      List.for_all (fun f -> match M.color f with C.AdjSUN _ -> true | _ -> false) f_list

    let two_adjoints_couple_to_singlets () =
      let vertices3, vertices4, verticesn = M.vertices () in
      List.exists (fun ((f1, f2, f3), _, _) ->
        match M.color f1, M.color f2, M.color f3 with
        | C.AdjSUN _, C.AdjSUN _, C.Singlet
        | C.AdjSUN _, C.Singlet, C.AdjSUN _
        | C.Singlet, C.AdjSUN _, C.AdjSUN _ -> true
        | _ -> false) vertices3 ||
      List.exists (fun ((f1, f2, f3, f4), _, _) ->
        match M.color f1, M.color f2, M.color f3, M.color f4 with
        | C.AdjSUN _, C.AdjSUN _, C.Singlet, C.Singlet
        | C.AdjSUN _, C.Singlet, C.AdjSUN _, C.Singlet
        | C.Singlet, C.AdjSUN _, C.AdjSUN _, C.Singlet
        | C.AdjSUN _, C.Singlet, C.Singlet, C.AdjSUN _
        | C.Singlet, C.AdjSUN _, C.Singlet, C.AdjSUN _
        | C.Singlet, C.Singlet, C.AdjSUN _, C.AdjSUN _ -> true
        | _ -> false) vertices4 ||
      List.exists (fun (flist, _, g) -> true) verticesn

    let external_ghosts f_list =
      if pure_adjoints f_list then
        two_adjoints_couple_to_singlets ()
      else
        true

(* We use [List.hd] and [List.tl] instead of pattern matching, because we
   consume [ecf_in] and [ecf_out] at a different pace. *)

    let tail_opt = function
      | [] -> []
      | _ :: tail -> tail

    let head_req = function
      | [] ->
          invalid_arg "Colorize.It().colorize_crossed_amplitude1: insufficient flows"
      | x :: _ -> x

    let rec colorize_crossed_amplitude1 ghosts acc f_list (ecf_in, ecf_out) =
      match f_list, ecf_in, ecf_out with
      | [], [], [] -> [List.rev acc]
      | [], _, _ ->
          invalid_arg "Colorize.It().colorize_crossed_amplitude1: leftover flows"
      | f :: rest, _, _ ->
          begin match M.color f with
          | C.Singlet ->
              colorize_crossed_amplitude1 ghosts
                (White f :: acc)
                rest (ecf_in, ecf_out)
          | C.SUN nc ->
              if nc > 0 then
                colorize_crossed_amplitude1 ghosts
                  (CF_in (f, head_req ecf_in) :: acc)
                  rest (tail_opt ecf_in, ecf_out)
              else if nc < 0 then
                colorize_crossed_amplitude1 ghosts
                  (CF_out (f, head_req ecf_out) :: acc)
                  rest (ecf_in, tail_opt ecf_out)
              else
                su0 "colorize_flavor"
          | C.AdjSUN _ ->
              let ecf_in' = head_req ecf_in
              and ecf_out' = head_req ecf_out in
              if ecf_in' = ecf_out' then begin
                if ghosts then
                  colorize_crossed_amplitude1 ghosts
                    (CF_aux f :: acc)
                    rest (tail_opt ecf_in, tail_opt ecf_out)
                else
                  []
              end else
                colorize_crossed_amplitude1 ghosts
                  (CF_io (f, ecf_in', ecf_out') :: acc)
                  rest (tail_opt ecf_in, tail_opt ecf_out)
          end

    let colorize_crossed_amplitude1 ghosts f_list (ecf_in, ecf_out) =
      colorize_crossed_amplitude1 ghosts [] f_list (ecf_in, ecf_out)

    let colorize_crossed_amplitude f_list =
      ThoList.rev_flatmap
        (colorize_crossed_amplitude1 (external_ghosts f_list) f_list)
        (external_color_flows f_list)

    let cross_uncolored p_in p_out =
      (List.map M.conjugate p_in) @ p_out

    let uncross_colored n_in p_lists_colorized =
      let p_in_out_colorized = List.map (ThoList.splitn n_in) p_lists_colorized in
      List.map
        (fun (p_in_colored, p_out_colored) ->
          (List.map conjugate p_in_colored, p_out_colored))
        p_in_out_colorized

    let amplitude p_in p_out =
      uncross_colored
        (List.length p_in)
        (colorize_crossed_amplitude (cross_uncolored p_in p_out))

    (* The $-$-sign in the second component is redundant, but a Whizard convention. *)
    let indices = function
      | White _ -> Color.Flow.of_list [0; 0]
      | CF_in (_, c) -> Color.Flow.of_list [c; 0]
      | CF_out (_, c) -> Color.Flow.of_list [0; -c]
      | CF_io (_, c1, c2) -> Color.Flow.of_list [c1; -c2]
      | CF_aux f -> Color.Flow.ghost ()

    let flow p_in p_out =
      (List.map indices p_in, List.map indices p_out)

  end

(* \thocwmodulesection{Colorizing a Monochrome Gauge Model} *)

module Gauge (M : Model.Gauge) = 
  struct

    let rcs = RCS.rename rcs_file "Colorize.Gauge()"
        [ "Colorizing Monochrome Gauge Models"]

    module CM = It(M)

    type flavor = CM.flavor
    type flavor_sans_color = CM.flavor_sans_color
    type gauge = CM.gauge
    type constant = CM.constant
    module Ch = CM.Ch
    let charges = CM.charges
    let flavor_sans_color = CM.flavor_sans_color
    let color = CM.color
    let pdg = CM.pdg
    let lorentz = CM.lorentz
    let propagator = CM.propagator
    let width = CM.width
    let conjugate = CM.conjugate
    let conjugate_sans_color = CM.conjugate_sans_color
    let fermion = CM.fermion
    let max_degree = CM.max_degree
    let vertices = CM.vertices
    let fuse2 = CM.fuse2
    let fuse3 = CM.fuse3
    let fuse = CM.fuse
    let flavors = CM.flavors
    let nc = CM.nc
    let external_flavors = CM.external_flavors
    let goldstone = CM.goldstone
    let parameters = CM.parameters
    let flavor_of_string = CM.flavor_of_string
    let flavor_to_string = CM.flavor_to_string
    let flavor_to_TeX = CM.flavor_to_TeX
    let flavor_symbol = CM.flavor_symbol
    let gauge_symbol = CM.gauge_symbol
    let mass_symbol = CM.mass_symbol
    let width_symbol = CM.width_symbol
    let constant_symbol = CM.constant_symbol
    let options = CM.options

    let incomplete s =
      failwith ("Colorize.Gauge()." ^ s ^ " not done yet!")

    type matter_field = M.matter_field
    type gauge_boson = M.gauge_boson
    type other = M.other

    type field =
      | Matter of matter_field
      | Gauge of gauge_boson
      | Other of other

    let field f = 
      incomplete "field"

    let matter_field f =
      incomplete "matter_field"

    let gauge_boson f =
      incomplete "gauge_boson"

    let other f =
      incomplete "other"

    let amplitude = CM.amplitude

    let flow = CM.flow

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
