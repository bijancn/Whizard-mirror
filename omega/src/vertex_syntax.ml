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

type index = int

(* \begin{dubious}
     \emph{The following is not complete yet.}
     We would like to allow scalars as coefficients of vectors.  Since
     recursive functors are not available in O'Caml yet, we will have to
     go back to a polymorphic implementation.
  \end{dubious} *)

module R = Algebra.Make_Ring(Algebra.Small_Rational)(Algebra.Term)
module V = Algebra.Make_Linear(R)

type scalar_current = S | P | SL | SR
type vector_current = V | A | VL | VR
type tensor_current = T

let scalar_current_to_string = function
  | S -> "S"
  | P -> "P"
  | SL -> "S-P"
  | SR -> "S+P"

let vector_current_to_string = function
  | V -> "V"
  | A -> "A"
  | VL -> "V-A"
  | VR -> "V+A"

let tensor_current_to_string = function
  | T -> "T"

type atom = 
  | I
  | Constant of string
  | Scalar_Current of scalar_current * index * index (* $\bar\psi_n\Gamma\psi_m$ *)
  | Dot of vatom * vatom
  | Eps of vatom * vatom * vatom * vatom

and vatom =
  | Polarization of index (* $\epsilon_n^{\mu_n}$ *)
  | Momentum of index (* $k_n^{\mu_n}$ *)
  | Vector_Current of vector_current * index * index (* $\bar\psi_n\fmslash{v}\psi_m$ *)
  | External of string
  | Pseudo of vatom * vatom * vatom (* $\epsilon^{\mu\nu\rho\sigma}v_\nu v_\rho v_\sigma$ *)
  | Vector_Sum of vector

and tatom =
  | Tensor_Current of tensor_current * index * index (* $\bar\psi_n\sigma_{\mu\nu}\psi_m$ *)
  | External_Tensor of string
  | Tensor_Sum of tensor

and satom =
  | Ket of index (* $\psi_{n}$ *)
  | Scalar_Ket of scalar_current * index (* $\Gamma\psi_{n}$ *)
  | Slash_Vector_Ket of vatom * vector_current * index (* $\fmslash{v}\psi_{n}$ *)
  | Spinor_Sum of spinor

and catom =
  | Bra of index (* $\bar\psi_{n}$ *)
  | Bra_Scalar of scalar_current * index (* $\bar\psi_{n}\Gamma$ *)
  | Bra_Slash_Vector of vatom * vector_current * index (* $\bar\psi_{n}\fmslash{v}$ *)
  | Conj_Spinor_Sum of conj_spinor

and vsatom =
  | Vector_Ket of vector_current * index (* $\gamma_{\mu}\psi_{n}$ *)
  | Vector_Spinor of vatom * satom (* $v_{\mu}\psi_{n}$ *)
  | Vector_Spinor_Sum of vector_spinor

and vcatom =
  | Bra_Vector of vector_current * index (* $\bar\psi_{n}\gamma_\mu$ *)
  | Vector_Conj_Spinor of vatom * catom (* $v_{\mu}\bar\psi_{n}$ *)
  | Vector_Conj_Spinor_Sum of vector_conj_spinor

and scalar = atom R.t
and vector = (vatom, atom) V.t
and tensor = (tatom, atom) V.t
and spinor = (satom, atom) V.t
and conj_spinor = (catom, atom) V.t
and vector_spinor = (vsatom, atom) V.t
and vector_conj_spinor = (vcatom, atom) V.t

let null = R.null
let integer i = R.scale (R.C.make i 1) (R.unit ())
let fraction x i = R.scale (R.C.make 1 i) x
let multiple i x = R.scale (R.C.make i 1) x
let mul = R.mul
let add = R.add
let sub = R.sub

let rec vatom_vsatom v = function
  | Vector_Ket (c, n) -> V.atom (Slash_Vector_Ket (v, c, n))
  | Vector_Spinor (v', s) -> V.singleton (R.atom (Dot (v, v'))) s
  | Vector_Spinor_Sum vss ->
      V.map (fun vs c -> V.scale c (vatom_vsatom v vs)) vss

let rec vatom_vcatom v = function
  | Bra_Vector (c, n) -> V.atom (Bra_Slash_Vector (v, c, n))
  | Vector_Conj_Spinor (v', c) -> V.singleton (R.atom (Dot (v, v'))) c
  | Vector_Conj_Spinor_Sum vss ->
      V.map (fun vs c -> V.scale c (vatom_vcatom v vs)) vss

(* The polymorphic map [Pmap] could use a full-fledged sibling
   polymorphic set [Pset], but for now we're satiesfied with a
   projection from [Pmap]: *)

module PM = Pmap.List

module type Pset =
  sig
    type 'a t
    val empty : 'a t
    val singleton : 'a -> 'a t
    val add : 'a -> 'a t -> 'a t
    val of_list : 'a list -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val elements : 'a t -> 'a list
  end

module PS =
  struct
    type 'a t = ('a, unit) PM.t
    let empty = PM.empty
    let singleton e = PM.singleton e ()
    let add e s = PM.add compare e () s
    let of_list list = List.fold_right add list empty
    let union s1 s2 = PM.union compare (fun () () -> ()) s1 s2
    let elements s = List.map fst (PM.elements s)
  end

type atoms =
    { constants : string list;
      momenta : index list;
      polarizations : index list;
      external_momenta : string list;
      spinors : index list;
      conj_spinors : index list }

type atoms_set =
    { constants_set : string PS.t;
      momenta_set : index PS.t;
      polarizations_set : index PS.t;
      external_momenta_set : string PS.t;
      spinors_set : index PS.t;
      conj_spinors_set : index PS.t }

let empty_atoms = 
  { constants_set = PS.empty;
    momenta_set = PS.empty;
    polarizations_set = PS.empty;
    external_momenta_set = PS.empty;
    spinors_set = PS.empty;
    conj_spinors_set = PS.empty }

let atoms_union a1 a2 = 
  { constants_set = PS.union a1.constants_set a2.constants_set;
    momenta_set = PS.union a1.momenta_set a2.momenta_set;
    polarizations_set = PS.union a1.polarizations_set a2.polarizations_set;
    external_momenta_set = PS.union a1.external_momenta_set a2.external_momenta_set;
    spinors_set = PS.union a1.spinors_set a2.spinors_set;
    conj_spinors_set = PS.union a1.conj_spinors_set a2.conj_spinors_set }

let rec atom_atoms = function
  | I -> empty_atoms
  | Constant s ->
      { empty_atoms with constants_set = PS.singleton s }
  | Scalar_Current (c, n, m) ->
      { empty_atoms with
        conj_spinors_set = PS.singleton n;
        spinors_set = PS.singleton m }
  | Dot (v1, v2) ->
      atoms_union (vatom_atoms v1) (vatom_atoms v2)
  | Eps (v1, v2, v3, v4) ->
      atoms_union
        (atoms_union (vatom_atoms v1) (vatom_atoms v2))
        (atoms_union (vatom_atoms v3) (vatom_atoms v4))

and scalar_atoms e =
  List.fold_right atoms_union
    (List.map atom_atoms (R.atoms e)) empty_atoms

and vatom_atoms = function
  | Vector_Current (c, n, m) ->
      { empty_atoms with
        conj_spinors_set = PS.singleton n;
        spinors_set = PS.singleton m }
  | External e ->
      { empty_atoms with external_momenta_set = PS.singleton e }
  | Polarization e ->
      { empty_atoms with polarizations_set = PS.singleton e }
  | Momentum p ->
      { empty_atoms with momenta_set = PS.singleton p }
  | Pseudo (v1, v2, v3) ->
      atoms_union (vatom_atoms v1) (atoms_union (vatom_atoms v3) (vatom_atoms v3))
  | Vector_Sum vector -> vector_atoms vector

and vector_atoms e =
  let vectors, scalars = V.atoms e in
  List.fold_right atoms_union
    (List.map vatom_atoms vectors @ List.map atom_atoms scalars)
    empty_atoms

let scalar_atoms e =
  let a = scalar_atoms e in
  { constants = PS.elements a.constants_set;
    momenta = PS.elements a.momenta_set;
    polarizations = PS.elements a.polarizations_set;
    external_momenta = PS.elements a.external_momenta_set;
    spinors = PS.elements a.spinors_set;
    conj_spinors = PS.elements a.conj_spinors_set }
    
let rec atom_to_string = function
  | I -> "i"
  | Constant s -> s
  | Scalar_Current (c, n, m) ->
      Printf.sprintf "<%d|%s|%d>" n (scalar_current_to_string c) m
  | Dot (v1, v2) ->
      "(" ^ vatom_to_string v1 ^ "." ^ vatom_to_string v2 ^ ")"
  | Eps (v1, v2, v3, v4) ->
      "eps(" ^ vatom_to_string v1 ^ "," ^ vatom_to_string v2  ^ "," ^
      vatom_to_string v3 ^ "," ^ vatom_to_string v4 ^ ")"

and vatom_to_string = function 
  | Polarization n -> "e" ^ string_of_int n
  | Momentum n -> "k" ^ string_of_int n
  | Vector_Current (c, n, m) ->
      Printf.sprintf "<%d|%s|%d>" n (vector_current_to_string c) m
  | External p -> "<<" ^ p ^ ">>"
  | Pseudo (v1, v2, v3) ->
      "eps(" ^ vatom_to_string v1 ^ "," ^ vatom_to_string v2  ^ "," ^
      vatom_to_string v3 ^ ")"
  | Vector_Sum vs -> vector_to_string vs

and satom_to_string = function
  | Ket i -> "|" ^ string_of_int i ^ ">"
  | Scalar_Ket (c, i) -> scalar_current_to_string c ^ "|" ^ string_of_int i ^ ">"
  | Slash_Vector_Ket (v, c, i) ->
      vatom_to_string v ^ "." ^ vector_current_to_string c ^ "|" ^ string_of_int i ^ ">"
  | Spinor_Sum s -> spinor_to_string s

and catom_to_string = function
  | Bra i -> "<" ^ string_of_int i ^ "|"
  | Bra_Scalar (c, i) -> "<" ^ string_of_int i ^ "|" ^ scalar_current_to_string c
  | Bra_Slash_Vector (v, c, i) ->
      "<" ^ string_of_int i ^ "|" ^ vector_current_to_string c ^ "." ^ vatom_to_string v
  | Conj_Spinor_Sum s -> conj_spinor_to_string s

and scalar_to_string s =
  R.to_string atom_to_string s

and vector_to_string v =
  V.to_string vatom_to_string atom_to_string v

and spinor_to_string v =
  V.to_string satom_to_string atom_to_string v

and conj_spinor_to_string v =
  V.to_string catom_to_string atom_to_string v

let incomplete f =
  failwith (f ^ ": incomplete")

let derive_atom_vatom v = function
  | I | Constant _ | Scalar_Current _ -> V.null ()
  | Dot (v1, v2) ->
      let v1a = V.atom v1
      and v2a = V.atom v2 in
      if v1 = v then begin
        if v2 = v then
          V.add v1a v2a
        else
          v2a
      end else begin
        if v2 = v then
          v1a
        else
          V.null ()
      end
  | Eps (v', v1, v2, v3) when v' = v -> V.atom (Pseudo (v1, v2, v3))
  | Eps (v1, v', v3, v2) when v' = v -> V.atom (Pseudo (v1, v2, v3))
  | Eps (v2, v3, v', v1) when v' = v -> V.atom (Pseudo (v1, v2, v3))
  | Eps (v3, v2, v1, v') when v' = v -> V.atom (Pseudo (v1, v2, v3))
  | Eps (_, _, _, _) -> V.null ()

(* \begin{subequations}
     \begin{align}
       \frac{\partial}{\partial\psi_k} \bar\psi_{n}\Gamma\psi_{m}
          &= \delta_{km} \bar\psi_{n}\Gamma \\
       \frac{\partial}{\partial\psi_k} v^{\mu} \bar\psi_{n}\gamma_{\mu}\psi_{m}
          &= \delta_{km} \bar\psi_{n}\fmslash{v}
           = v^{\mu} \frac{\partial}{\partial\psi_k} \bar\psi_{n}\gamma_{\mu}\psi_{m} \\
       \ldots
     \end{align}
   \end{subequations} *)
let rec derive_atom_satom s = function
  | I | Constant _ -> V.null ()
  | Scalar_Current (c, n, m) when m = s -> V.atom (Bra_Scalar (c, n))
  | Scalar_Current (_, _, _) -> V.null ()
  | Dot (v1, v2) ->
      let dv1 = derive_vatom_satom s v1
      and dv2 = derive_vatom_satom s v2 in
      begin match dv1, dv2 with
      | None, None -> V.null ()
      | Some (Bra_Vector (c, n)), None ->
          V.atom (Bra_Slash_Vector (v2, c, n))
      | Some (Vector_Conj_Spinor (v, c)), None ->
          V.singleton (R.atom (Dot (v, v2))) c
      | Some (Vector_Conj_Spinor_Sum _), None ->
          incomplete "derive_atom_satom"
      | None, Some (Bra_Vector (c, n)) ->
          V.atom (Bra_Slash_Vector (v1, c, n))
      | None, Some (Vector_Conj_Spinor (v, c)) ->
          V.singleton (R.atom (Dot (v, v1))) c
      | None, Some (Vector_Conj_Spinor_Sum _) ->
          incomplete "derive_atom_satom"
      | Some vs1, Some vs2 ->
          incomplete "derive_atom_satom"
      end
  | Eps (_, _, _, _) ->
      incomplete "derive_atom_satom"

(* \begin{subequations}
     \begin{align}
       \frac{\partial}{\partial\psi_k} \bar\psi_{n}\gamma_{\mu}\psi_{m}
          &= \delta_{km} \bar\psi_{n}\gamma_{\mu} \\
       \ldots
     \end{align}
   \end{subequations} *)
and derive_vatom_satom s = function
  | Polarization _ | Momentum _ -> None
  | Vector_Current (c, n, m) when m = s -> Some (Bra_Vector (c, n))
  | Vector_Current (_, _, _) -> None
  | External _ -> None
  | Pseudo _ -> incomplete "derive_vatom_satom"
  | Vector_Sum vs ->
      Some (Vector_Conj_Spinor_Sum (derive_vsatom s vs))

and derive_vsatom s vs =
  incomplete "derive_vsatom"

(* \begin{subequations}
     \begin{align}
       \frac{\partial}{\partial\bar\psi_k} \bar\psi_{n}\Gamma\psi_{m}
          &= \delta_{kn} \Gamma\psi_{m} \\
       \frac{\partial}{\partial\bar\psi_k} v^{\mu} \bar\psi_{n}\gamma_{\mu}\psi_{m}
          &= \delta_{kn} \fmslash{v}\psi_{m}
           = v^{\mu} \frac{\partial}{\partial\bar\psi_k} \bar\psi_{n}\gamma_{\mu}\psi_{m} \\
       \ldots
     \end{align}
   \end{subequations} *)
let rec derive_atom_catom s = function
  | I | Constant _ -> V.null ()
  | Scalar_Current (c, n, m) when n = s -> V.atom (Scalar_Ket (c, m))
  | Scalar_Current (_, _, _) -> V.null ()
  | Dot (v1, v2) ->
      begin match derive_vatom_catom s v1, derive_vatom_catom s v2 with
      | None, None -> V.null ()
      | Some (Vector_Ket (c, n)), None ->
          V.atom (Slash_Vector_Ket (v2, c, n))
      | Some (Vector_Spinor (v, s)), None ->
          V.singleton (R.atom (Dot (v, v2))) s
      | Some (Vector_Spinor_Sum _), None ->
          incomplete "derive_atom_catom"
      | None, Some (Vector_Ket (c, n)) ->
          V.atom (Slash_Vector_Ket (v1, c, n))
      | None, Some (Vector_Spinor (v, s)) ->
          V.singleton (R.atom (Dot (v, v1))) s
      | None, Some (Vector_Spinor_Sum _) ->
          incomplete "derive_atom_catom"
      | Some vs1, Some vs2 ->
          incomplete "derive_atom_catom"
      end
  | Eps (_, _, _, _) ->
      incomplete "derive_atom_catom"

(* \begin{subequations}
     \begin{align}
       \frac{\partial}{\partial\bar\psi_k} \bar\psi_{n}\gamma_{\mu}\psi_{m}
          &= \delta_{kn} \gamma_{\mu}\psi_{m} \\
       \ldots
     \end{align}
   \end{subequations} *)
and derive_vatom_catom s = function
  | Polarization _ -> None
  | Momentum _ -> None
  | Vector_Current (c, n, m) when n = s -> Some (Vector_Ket (c, m))
  | Vector_Current (_, _, _) -> None
  | External _ -> None
  | Pseudo _ -> incomplete "derive_vatom_catom"
  | Vector_Sum vs ->
      Some (Vector_Spinor_Sum (derive_vcatom s vs))

and derive_vcatom s vs =
  incomplete "derive_vcatom"

let e i = Polarization i
let k i = Momentum i
let x s = External s

let dot v1 v2 = 
  R.atom (if v1 <= v2 then Dot (v1, v2) else Dot (v2, v1))

let eps v1 v2 v3 v4 = 
  R.atom (Eps (v1, v2, v3, v4))

let pseudo v1 v2 v3 = 
  Pseudo (v1, v2, v3)

let contract_left v t =
  invalid_arg "contractions of tensor currents not implemented yet"

let contract_right t v =
  invalid_arg "contractions of tensor currents not implemented yet"

let addv v1 v2 = 
  Vector_Sum (V.add (V.atom v1) (V.atom v2))

let subv v1 v2 = 
  Vector_Sum (V.sub (V.atom v1) (V.atom v2))

let scalar_current c i j =
  R.atom (Scalar_Current (c, i, j))

let vector_current c i j =
  Vector_Current (c, i, j)

let tensor_current c i j =
  Tensor_Current (c, i, j)

let i () = R.atom I

let constant s =
  R.atom (Constant s)

let partial_vector v s =
  V.partial (derive_atom_vatom v) s

let partial_spinor i s =
  V.partial (derive_atom_satom i) s

let partial_conj_spinor i s =
  V.partial (derive_atom_catom i) s

(*i
let scalev c v = 
  Sum (V.scale (V.C.atom c) (V.atom v))
i*)

exception Syntax_Error of string * int * int

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  compile-command:"ocamlc -o vertex thoList.ml{i,} pmap.ml{i,} vertex.ml"
 *  End:
i*)
