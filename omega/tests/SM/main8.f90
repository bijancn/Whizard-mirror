! $Id: main8.f90 1203 2009-10-08 11:39:11Z jr_reuter $

program main8

  use kinds
  use tao_random_numbers
  use kinematics
  use testbed_old
  use rambo
  use omega_amplitudes8
  ! use omega_helas_amplitudes
  use madgraph8
  implicit none

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  call export_parameters_to_madgraph ()

  call check8_madgraph ("e+ e- -> mu- numubar tau+ nutau u ubar", n, &
       oepem_muvmtavtuub, sepem_muvmtavtuub, epem_muvmtavtuub, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), 0.0_default, &
          mass(15), 0.0_default, mass(2), mass(2) /), &
       tolerance = tolerance, mode = mode)

  call check8_madgraph ("e+ e- -> nue nuebar mu- numubar u dbar", n, &
       oepem_vevemuvmudb, sepem_vevemuvmudb, epem_vevemuvmudb, real (roots, kind=default), &
       (/ mass(11), mass(11), 0.0_default, 0.0_default, &
          mass(13), 0.0_default, mass(2), mass(1) /), &
       tolerance = tolerance, mode = mode)

  call check8_madgraph ("e+ e- -> mu+ mu- tau+ tau- u ubar", n, &
       oepem_mumutatauub, sepem_mumutatauub, epem_mumutatauub, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), mass(13), mass(15), mass(15), mass(2), mass(2) /), &
       tolerance = tolerance, mode = mode)

  call check8_madgraph ("e+ e- -> e+ e- A A A A", n, &
       oepem_epemaaaa, sepem_epemaaaa, epem_epemaaaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(11), &
          0.0_default, 0.0_default, 0.0_default , 0.0_default /), &
       tolerance = tolerance, mode = mode)

end program main8




