! main5.f90 --

program main5
  use kinds
  use tao_random_numbers
  use testbed_old
  use rambo
  use omega_amplitudes5
  ! use omega_helas_amplitudes
  use madgraph5

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  call export_parameters_to_madgraph ()

  call check5_madgraph ("e+ e- -> W+ W- Z", n, &
       oepem_wpwmz, sepem_wpwmz, epem_wpwmz, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(24), mass(24), mass(23) /), &
       states = (/ 2, 2, 3, 3, 3 /), tolerance = tolerance, mode = mode)

  call check5_madgraph ("e+ e- -> W+ W- A", n, &
       oepem_wpwma, sepem_wpwma, epem_wpwma, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(24), mass(24), 0.0_default /), &
       states = (/ 2, 2, 3, 3, 2 /), tolerance = tolerance, mode = mode)

  call check5_madgraph ("e- e+ -> e- nuebar W+", n, &
       oemep_emvewp, semep_emvewp, emep_emvewp, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), 0.0_default, mass(24) /), &
       states = (/ 2, 2, 2, 2, 3 /), tolerance = tolerance, mode = mode)

  call check5_madgraph ("e+ e- -> e+ e- A", n, oepem_epema, sepem_epema, epem_epema, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), 0.0_default /), &
       tolerance = tolerance, mode = mode)

  call check5_madgraph ("e- e- -> e- e- A", n, oemem_emema, semem_emema, emem_emema, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), 0.0_default /), &
       symmetry = reshape ((/ -1, 1, 2, -1, 3, 4 /), (/ 3, 2/)), &
       tolerance = tolerance, mode = mode)

  call check5_madgraph ("e+ e- -> A A A", n, oepem_aaa, sepem_aaa, epem_aaa, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 3, 4, 1, 3, 5, 1, 4, 5 /), (/ 3, 3/)), &
       tolerance = tolerance, mode = mode)

  call check5_madgraph ("e+ e- -> Z A A", n, oepem_zaa, sepem_zaa, epem_zaa, &
       real (roots, kind=default), &
       (/ mass(11), mass(11), mass(23), 0.0_default, 0.0_default /), &
       states = (/ 2, 2, 3, 2, 2 /), &
       symmetry = reshape ((/ 1, 4, 5 /), (/ 3, 1/)), tolerance = tolerance, mode = mode)

end program main5


