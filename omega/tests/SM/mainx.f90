! mainx.f90 --

program mainx
  use kinds
  use tao_random_numbers
  use testbed_old
  use rambo
  use omega_amplitudesx
  ! use omega_helas_amplitudes
  use madgraphx
  implicit none

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  !!! mass(1:19) = 0
  call export_parameters_to_madgraph ()

  !!! call compare_sum8_madgraph (n, oepem_epvebbbdub, sepem_epvebbbdub, &
  !!!      real (roots, kind=default), &
  !!!      (/ mass(11), mass(11), mass(11), mass(12), mass(5), mass(5), mass(1), mass(2) /), &
  !!!      tolerance = tolerance, mode = mode)
  !!! 
  !!! stop

  call check8_madgraph ("e+ e- -> e+ nue b bbar d ubar", n, &
       oepem_epvebbbdub, sepem_epvebbbdub, epem_epvebbbdub, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(11), mass(12), mass(5), mass(5), mass(1), mass(2) /), &
       tolerance = tolerance, mode = mode)

  call check5_madgraph ("W+ W- -> W+ W- A", n, &
       owpwm_wpwma, swpwm_wpwma, wpwm_wpwma, real (roots, kind=default), &
       (/ mass(24), mass(24), mass(24), mass(24), 0.0_default /), &
       states = (/ 3, 3, 3, 3, 2 /), tolerance = tolerance, mode = mode)

  call check5_madgraph ("W+ W- -> A A A", n, &
       owpwm_aaa, swpwm_aaa, wpwm_aaa, real (roots, kind=default), &
       (/ mass(24), mass(24), 0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 3, 4, 1, 3, 5, 1, 4, 5  /), (/ 3, 3/)), &
       states = (/ 3, 3, 2, 2, 2 /), tolerance = tolerance, mode = mode)

  call check5_madgraph ("W+ W- -> Z A A", n, &
       owpwm_zaa, swpwm_zaa, wpwm_zaa, real (roots, kind=default), &
       (/ mass(24), mass(24), mass(23), 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 4, 5  /), (/ 3, 1/)), &
       states = (/ 3, 3, 3, 2, 2 /), tolerance = tolerance, mode = mode)

  call check6_madgraph ("e+ e- -> W+ W- A A", n, &
       oepem_wpwmaa, sepem_wpwmaa, epem_wpwmaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(24), mass(24), 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ 1, 5, 6 /), (/ 3, 1/)), &
       states = (/ 2, 2, 3, 3, 2, 2 /), tolerance = tolerance, mode = mode)

  call check8_madgraph ("e+ e- -> mu- numubar tau+ nutau A A", n, &
       oepem_muvmtavtaa, sepem_muvmtavtaa, epem_muvmtavtaa, real (roots, kind=default), &
       (/ mass(11), mass(11), mass(13), 0.0_default, &
          mass(15), 0.0_default, 0.0_default , 0.0_default /), &
       symmetry = reshape ((/ 1, 7, 8 /), (/ 3, 1/)), &
       tolerance = tolerance, mode = mode)

end program mainx





