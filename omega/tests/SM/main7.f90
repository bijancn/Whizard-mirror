! main7.f90 --

program main7

  use kinds
  use tao_random_numbers
  use kinematics
  use testbed_old
  use rambo
  use omega_amplitudes7
  ! use omega_helas_amplitudes
  use madgraph7
 
  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  call export_parameters_to_madgraph ()

  call check7_madgraph ("e+ e- -> nue nuebar u ubar Z", n, &
       oepem_veveuubz, sepem_veveuubz, epem_veveuubz, real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, mass(2), mass(2), mass(23) /), &
       states = (/ 2, 2, 2, 2, 2, 2, 3 /), tolerance = tolerance, mode = mode)

  call check7_madgraph ("e- e+ -> e- nuebar u dbar A (2 groves)", n, &
       oemep_emveudba_groves, semep_emveudba, emep_emveudba, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), 0.0_default, mass(2), mass(1), 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("e- e+ -> e- nuebar u dbar A", n, &
       oemep_emveudba, semep_emveudba, emep_emveudba, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), 0.0_default, mass(2), mass(1), 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("e+ e- -> mu- numubar tau+ nutau A", n, &
       oepem_muvmtavta, sepem_muvmtavta, epem_muvmtavta, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), 0.0_default, &
          mass(15), 0.0_default, 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("e+ e- -> e+ e- e+ e- A", n, &
       oepem_epemepema, sepem_epemepema, epem_epemepema, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), mass(11), mass(11), 0.0_default /), &
       symmetry = reshape ((/ -1, 3, 5, -1, 4, 6 /), (/ 3, 2/)), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("e+ e- -> e+ e- A A A", n, &
       oepem_epemaaa, sepem_epemaaa, epem_epemaaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), &
          0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 5, 6, 1, 5, 7, 1, 6, 7 /), (/ 3, 3/)), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("e+ e- -> A A A A A", n, &
       oepem_aaaaa, sepem_aaaaa, epem_aaaaa, real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, &
          0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 3, 4, 1, 3, 5, 1, 3, 6, 1, 3, 7, &
                              1, 4, 5, 1, 4, 6, 1, 4, 7, 1, 5, 6, &
                              1, 5, 7, 1, 6, 7 /), (/ 3, 10/)), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("A A -> e+ e- A A A", n, &
       oaa_epemaaa, saa_epemaaa, aa_epemaaa, real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, mass(11), mass(11), &
          0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 1, 2, 1, 5, 6, 1, 5, 7, 1, 6, 7 /), (/ 3, 4/)), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("A A -> e+ e- mu+ mu- A", n, &
       oaa_epemmumua, saa_epemmumua, aa_epemmumua, real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, mass(11), mass(11), &
          mass(13), mass(13), 0.0_default /), &
       symmetry = reshape ((/ 1, 1, 2 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

  call check7_madgraph ("A A -> e+ e- e+ e- A", n, &
       oaa_epemepema, saa_epemepema, aa_epemepema, real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, mass(11), mass(11), &
          mass(11), mass(11), 0.0_default /), &
       symmetry = reshape ((/ 1, 1, 2, -1, 3, 5, -1, 4, 6 /), (/ 3, 3/)), &
       tolerance = tolerance, mode = mode)

end program main7




