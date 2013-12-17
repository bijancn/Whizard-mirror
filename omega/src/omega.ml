(* $Id: omega.ml 4997 2013-12-13 13:22:56Z ohl $

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

let (<<) f g x = f (g x)
let (>>) f g x = g (f x)

let opt_array_to_list a =
  let rec opt_array_to_list' acc i a =
    if i < 0 then
      acc
    else
      begin match a.(i) with
      | None -> opt_array_to_list' acc (pred i) a
      | Some x -> opt_array_to_list' (x :: acc) (pred i) a
      end in
  opt_array_to_list' [] (Array.length a - 1) a
  
module P = Momentum.Default
module P_Whizard = Momentum.DefaultW

module type T =
  sig
    val main : unit -> unit
    type flavor
    val diagrams : flavor -> flavor -> flavor list ->
      ((flavor * Momentum.Default.t) *
         (flavor * Momentum.Default.t,
          flavor * Momentum.Default.t) Tree.t) list
  end

module Make (Fusion_Maker : Fusion.Maker) (Target_Maker : Target.Maker) (M : Model.T) =
  struct

    module CM = Colorize.It(M)

    type flavor = M.flavor

    module Proc = Process.Make(M)

(* \begin{dubious}
     NB: this causes the constant initializers in [Fusion_Maker] more than once.
     Such side effects must be avoided if the initializers involve expensive
     computations.   \emph{Relying on the fact that the functor will be
     called only once is not a good idea!}
   \end{dubious} *)
    module F = Fusion_Maker(P)(M)
    module CF = Fusion.Multi(Fusion_Maker)(P)(M)
    module T = Target_Maker(Fusion_Maker)(P)(M)
    module W = Whizard.Make(Fusion_Maker)(P)(P_Whizard)(M)
    module C = Cascade.Make(M)(P)

    module Tree_Projection =
      struct
	type wf = flavor * int list
	type base = wf list
	type elt = wf list * (wf * F.coupling option, wf) Tree.t
	let compare_elt = compare
	let compare_base = compare
	let pi (externals, tree) =
	  externals
      end

    module Sheaf = Bundle.Make (Tree_Projection)

(* \begin{dubious}
     If we plan to distiguish different couplings later on,
     we can no long map all instances of [coupling option]
     in the tree to [None].  In this case, we will need
     to normalize different fusion orders [Coupling.fuse2],
     [Coupling.fuse3] or [Coupling.fusen], because they would
     otherwise lead to inequivalent diagrams. Unfortunately, this
     stuff packaged deep in [Fusion.Tagged_Coupling].
   \end{dubious} *)

(*i
    let strip_fuse' = function
      | Coupling.V3 (v, f, c) -> Coupling.V3 (v, Coupling.F12, c)
      | Coupling.V4 (v, f, c) -> Coupling.V4 (v, Coupling.F123, c)
      | Coupling.Vn (v, f, c) -> Coupling.Vn (v, [], c)

    let strip_fuse = function
      | Some c -> Some (strip_fuse' c)
      | None -> None
i*)

(* \begin{dubious}
     The [Tree.canonicalize] below should be necessary to remove
     topologically equivalent duplicates.
   \end{dubious} *)

    let forest1 a =
      let wf_sans_color wf =
	(F.flavor_sans_color wf, F.momentum_list wf) in
      let wf1 = List.hd (F.externals a)
      and externals = List.map wf_sans_color (F.externals a) in
      List.map
	(fun t ->
	  (externals,
	   Tree.canonicalize
	     (Tree.map
		(fun (wf, c) -> (wf_sans_color wf, None)) wf_sans_color t)))
	(F.forest wf1 a)

    let forest amplitudes =
      ThoList.flatmap forest1 (CF.processes amplitudes)

    let sheaf amplitudes =
      Sheaf.fibers (Sheaf.of_list (forest amplitudes))

    let amplitudes_by_flavor amplitudes =
      List.map opt_array_to_list (Array.to_list (CF.process_table amplitudes))

    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    let format_p wf =
      String.concat "" (List.map p2s (F.momentum_list wf))

    let variable wf =
      M.flavor_to_string (F.flavor_sans_color wf) ^ "[" ^ format_p wf ^ "]"

    let variable' wf =
      CM.flavor_to_TeX (F.flavor wf) ^ "(" ^ format_p wf ^ ")"

    let amplitudes_to_feynmf latex name amplitudes =
      let fmf wf =
	{ Tree.style =
            begin match CM.propagator (F.flavor wf) with
            | Coupling.Prop_Feynman
            | Coupling.Prop_Gauge _ ->
              begin match CM.color (F.flavor wf) with
              | Color.AdjSUN _ -> Some ("gluon", "")
              | _ -> Some ("boson", "")
              end
            | Coupling.Prop_Col_Feynman -> Some ("gluon", "")
            | Coupling.Prop_Unitarity
            | Coupling.Prop_Rxi _ -> Some ("dbl_wiggly", "")
            | Coupling.Prop_Spinor
            | Coupling.Prop_ConjSpinor -> Some ("fermion", "")
            | _ -> None
            end;
          Tree.rev =
            begin match CM.propagator (F.flavor wf) with
            | Coupling.Prop_Spinor -> true
            | Coupling.Prop_ConjSpinor -> false
            | _ -> false
            end;
          Tree.label = None;
          Tree.tension = None } in
      Tree.to_feynmf latex name variable' format_p
	(List.fold_left
           (fun acc a ->
             match F.externals a with
             | wf1 :: wf2 :: wfs ->
               ("$ " ^
                   CM.flavor_to_TeX (F.flavor wf1) ^ " " ^
                   CM.flavor_to_TeX (F.flavor wf2) ^ " \\to " ^
                   String.concat " "
                   (List.map
                      (CM.flavor_to_TeX << CM.conjugate << F.flavor)
                      wfs) ^
                   " $",
		[wf1; wf2],
		(List.map
                   (Tree.map
                      (fun (n, _) ->
			let n' = fmf n in
			if List.mem n [wf1; wf2] then
                          { n' with Tree.rev = not n'.Tree.rev }
			else
                          n')
                      (fun l ->
			if List.mem l [wf1; wf2] then
                          l
			else
                          F.conjugate l))
                   (F.forest wf1 a))) :: acc
             | _ -> acc)
           [] (CF.processes amplitudes))
	
    let amplitudes_sans_color_to_feynmf latex name amplitudes =
      let momentum_to_TeX (_, p) =
	String.concat "" (List.map p2s p) in
      let wf_to_TeX (f, _ as wf) =
	M.flavor_to_TeX f ^ "(" ^ momentum_to_TeX wf ^ ")" in
      let fmf (f, p) =
	{ Tree.style =
            begin match M.propagator f with
            | Coupling.Prop_Feynman
            | Coupling.Prop_Gauge _ ->
              begin match M.color f with
              | Color.AdjSUN _ -> Some ("gluon", "")
              | _ -> Some ("boson", "")
              end
            | Coupling.Prop_Col_Feynman -> Some ("gluon", "")
            | Coupling.Prop_Unitarity
            | Coupling.Prop_Rxi _ -> Some ("dbl_wiggly", "")
            | Coupling.Prop_Spinor
            | Coupling.Prop_ConjSpinor -> Some ("fermion", "")
            | _ -> None
            end;
          Tree.rev =
            begin match M.propagator f with
            | Coupling.Prop_Spinor -> true
            | Coupling.Prop_ConjSpinor -> false
            | _ -> false
            end;
          Tree.label = None;
          Tree.tension = None } in
      Tree.to_feynmf latex name wf_to_TeX momentum_to_TeX
	(List.map
	   (fun (externals, trees) ->
	     begin match externals with
	     | wf1 :: wf2 :: wfs ->
               ("$ " ^
		   M.flavor_to_TeX (fst wf1) ^ " " ^
		   M.flavor_to_TeX (fst wf2) ^ " \\to " ^
		   String.concat " "
		   (List.map (M.flavor_to_TeX << M.conjugate << fst) wfs) ^
		   " $",
		[wf1; wf2],
		(List.map
		   (fun (_, tree) ->
		     (Tree.map
			(fun (n, c) ->
                          let n' = fmf n in
                          if List.mem n [wf1; wf2] then
			    { n' with Tree.rev = not n'.Tree.rev }
                          else
			    n')
			(fun (f, p) ->
                          if List.mem (f, p) [wf1; wf2] then
			    (f, p)
                          else
			    (M.conjugate f, p))
			tree))
		   trees))
	     | _ -> failwith "less than two eternal particles"
	     end)
	   (sheaf amplitudes))

    let version () =
      List.iter (fun s -> prerr_endline ("RCS: " ^ s))
        (ThoList.flatmap RCS.summary (CM.rcs :: T.rcs_list @ F.rcs_list))

    let debug (str, descr, opt, var) =
      [ "-warning:" ^ str, Arg.Unit (fun () -> var := (opt, false):: !var),
        "         check " ^ descr ^ " and print warning on error";
        "-error:" ^ str, Arg.Unit (fun () -> var := (opt, true):: !var),
        "           check " ^ descr ^ " and terminate on error" ]

    let rec include_goldstones = function
      | [] -> false
      | (T.Gauge, _) :: _ -> true
      | _ :: rest -> include_goldstones rest
      
    let read_lines_rev file =
      let ic = open_in file in
      let rev_lines = ref [] in
      let rec slurp () =
        rev_lines := input_line ic :: !rev_lines;
        slurp () in
      try
        slurp ()
      with
      | End_of_file ->
          close_in ic;
          !rev_lines

    let read_lines file = 
      List.rev (read_lines_rev file)

    type cache_mode =
      | Cache_Default
      | Cache_Initialize of string

    let cache_option =
      ref Cache_Default

    let unphysical_polarization = ref None
    
(* \thocwmodulesection{Main Program} *)

    let main () =
      let usage =
        "usage: " ^ Sys.argv.(0) ^
        " [options] [" ^
	  String.concat "|" (List.map M.flavor_to_string 
			       (ThoList.flatmap snd
				  (M.external_flavors ()))) ^ "]"
      and rev_scatterings = ref []
      and rev_decays = ref []
      and cascades = ref []
      and checks = ref []
      and output_file = ref None
      and print_forest = ref false
      and template = ref false
      and feynmf = ref None
      and feynmf_tex = ref false
      and feynmf_colored = ref false
      and quiet = ref false
      and write = ref true
      and params = ref false
      and poles = ref false
      and dag_out = ref None
      and dag0_out = ref None in
      Arg.parse
        (Options.cmdline "-target:" T.options @
         Options.cmdline "-model:" M.options @
         Options.cmdline "-fusion:" CF.options @
         ThoList.flatmap debug
           ["a", "arguments", T.All, checks;
            "n", "# of input arguments", T.Arguments, checks;
            "m", "input momenta", T.Momenta, checks;
            "g", "internal Ward identities", T.Gauge, checks] @
         [("-o", Arg.String (fun s -> output_file := Some s),
           "file             write to given file instead of /dev/stdout");
          ("-scatter",
           Arg.String (fun s -> rev_scatterings := s :: !rev_scatterings),
           "expr       in1 in2 -> out1 out2 ...");
          ("-scatter_file",
           Arg.String (fun s -> rev_scatterings := read_lines_rev s @ !rev_scatterings),
           "name  each line: in1 in2 -> out1 out2 ...");
          ("-decay", Arg.String (fun s -> rev_decays := s :: !rev_decays),
           "expr         in -> out1 out2 ...");
          ("-decay_file",
           Arg.String (fun s -> rev_decays := read_lines_rev s @ !rev_decays),
           "name    each line: in -> out1 out2 ...");
          ("-cascade", Arg.String (fun s -> cascades := s :: !cascades),
           "expr       select diagrams");
          ("-initialize",
           Arg.String (fun s -> cache_option := Cache_Initialize s),
           "dir     precompute lookup tables and store them in directory");
          ("-unphysical", Arg.Int (fun i -> unphysical_polarization := Some i),
           "n       use unphysical polarization for n-th particle / test WIs");
          ("-template", Arg.Set template,
           "          write a template for handwritten amplitudes");
          ("-forest", Arg.Set print_forest,
           "            Diagrammatic expansion");
          ("-feynmf", Arg.String (fun s -> feynmf := Some s),
           "file        print FeynMF/MP output");
          ("-feynmf_colored", Arg.Set feynmf_colored,
           "    draw Feynman diagrams for individual color flows");
          ("-feynmf_tex", Arg.Set feynmf_tex,
           "        enclose FeynMP output in LaTeX wrapper");
          ("-revision", Arg.Unit version,
           "          print revision control information");
          ("-quiet", Arg.Set quiet,
           "             don't print a summary");
          ("-summary", Arg.Clear write,
           "           print only a summary");
          ("-params", Arg.Set params,
           "            print the model parameters");
          ("-poles", Arg.Set poles,
           "             print the Monte Carlo poles");
          ("-dag", Arg.String (fun s -> dag_out := Some s),
           "               print minimal DAG");
          ("-full_dag", Arg.String (fun s -> dag0_out := Some s),
           "          print complete DAG")])
(*i       ("-T", Arg.Int Topology.Binary.debug_triplet, "");
          ("-P", Arg.Int Topology.Binary.debug_partition, "")])
i*)
        (fun _ -> prerr_endline usage; exit 1)
        usage;

      let cmdline =
        String.concat " " (List.map ThoString.quote (Array.to_list Sys.argv)) in
        
      let output_channel =
        match !output_file with
        | None -> stdout
        | Some name -> open_out name in

      let processes =
        try
          ThoList.uniq
            (List.sort compare 
               (match List.rev !rev_scatterings, List.rev !rev_decays with
               | [], [] -> []
               | scatterings, [] ->
                   Proc.expand_scatterings (List.map Proc.parse_scattering scatterings)
               | [], decays ->
                   Proc.expand_decays (List.map Proc.parse_decay decays)
               | scatterings, decays -> 
                   invalid_arg "mixed scattering and decay!"))
        with
        | Invalid_argument s ->
            begin 
              Printf.eprintf "O'Mega: invalid process specification: %s!\n" s;
              flush stderr;
              []
            end in

(* \begin{dubious}
     This is still crude.  Eventually, we want to catch \emph{all} exceptions
     and write an empty (but compilable) amplitude unless one of the special options
     is selected.
   \end{dubious} *)

      begin match processes, !cache_option, !params with
      | [], Cache_Initialize dir, false ->
          F.initialize_cache dir;
          exit 0
      | _, _, true ->
          T.parameters_to_channel output_channel;
          exit 0
      | [], _, false ->
          T.amplitudes_to_channel cmdline output_channel !checks CF.empty;
          exit 0
      | _, _, false ->

        let selectors =
          let fin, fout = List.hd processes in
          C.to_selectors (C.of_string_list (List.length fin + List.length fout) !cascades) in

        let amplitudes =
          try
            begin match F.check_charges () with
            | [] -> ()
            | violators ->
                let violator_strings =
                  String.concat ", "
                    (List.map
                       (fun flist ->
                         "(" ^ String.concat "," (List.map M.flavor_to_string flist) ^ ")")
                       violators) in
                failwith ("charge violating vertices: " ^ violator_strings)
            end;
            CF.amplitudes (include_goldstones !checks) !unphysical_polarization selectors processes
          with
          | exc ->
              begin 
                Printf.eprintf
                  "O'Mega: exception %s in amplitude construction!\n"
                  (Printexc.to_string exc);
                flush stderr;
                CF.empty;
              end in

        if !write then
          T.amplitudes_to_channel cmdline output_channel !checks amplitudes;

        if not !quiet then begin
          List.iter
            (fun amplitude ->
              Printf.eprintf "SUMMARY: %d fusions, %d propagators"
                (F.count_fusions amplitude) (F.count_propagators amplitude);
              flush stderr;
              Printf.eprintf ", %d diagrams" (F.count_diagrams amplitude);
              Printf.eprintf "\n")
            (CF.processes amplitudes);
        end;

        if !poles then begin
          List.iter
            (fun amplitude ->
              W.write output_channel "omega" (W.merge (W.trees amplitude)))
            (CF.processes amplitudes)
        end;

        begin match !dag0_out with
        | Some name ->
            let ch = open_out name in
            List.iter (F.tower_to_dot ch) (CF.processes amplitudes);
            close_out ch
        | None -> ()
        end;

        begin match !dag_out with
        | Some name ->
            let ch = open_out name in
            List.iter (F.amplitude_to_dot ch) (CF.processes amplitudes);
            close_out ch
        | None -> ()
        end;

        if !print_forest then
          List.iter
            (fun amplitude ->
              List.iter (fun t -> Printf.eprintf "%s\n"
                  (Tree.to_string
                     (Tree.map (fun (wf, _) -> variable wf) (fun _ -> "") t)))
                (F.forest (List.hd (F.externals amplitude)) amplitude))
            (CF.processes amplitudes);

        begin match !feynmf, !feynmf_colored with
        | Some name, true ->
	  amplitudes_to_feynmf feynmf_tex name amplitudes
        | Some name, false ->
	  amplitudes_sans_color_to_feynmf feynmf_tex name amplitudes
        | None, _ -> ()
        end;

        begin match !output_file with
        | None -> ()
        | Some name -> close_out output_channel
        end;

        exit 0

      end

(* \begin{dubious}
     This was only intended for debugging O'Giga \ldots
   \end{dubious} *)

    let decode wf =
      (F.flavor wf, (F.momentum wf : Momentum.Default.t))

    let diagrams in1 in2 out =
      match F.amplitudes false C.no_cascades [in1; in2] out with
      | a :: _ ->
          let wf1 = List.hd (F.externals a)
          and wf2 = List.hd (List.tl (F.externals a)) in
          let wf2 = decode wf2 in
          List.map (fun t ->
            (wf2,
             Tree.map (fun (wf, _) -> decode wf) decode t))
            (F.forest wf1 a)
      | [] -> []

    let diagrams in1 in2 out =
      failwith "Omega().diagrams: disabled"

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
