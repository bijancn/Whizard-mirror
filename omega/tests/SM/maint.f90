! maint.f90 --

program maint
  use kinds
  use tao_random_numbers
  use testbed_old
  use rambo
  use omega_amplitudest
  use madgraph4
  implicit none

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)
  call export_parameters_to_madgraph ()

  call check4_madgraph ("e+ e- -> W+ W-", n, oepem_wpwm, sepem_wpwm, epem_wpwm, &
       real (roots, kind=default), (/ mass(11), mass(11), mass(24), mass(24) /), &
       states = (/ 2, 2, 3, 3 /), tolerance = tolerance, mode = mode)
  
  ! call check8_madgraph ("e+ e- -> e+ nue b bbar d ubar", n, &
  !      oepem_epvebbbdub, sepem_epvebbbdub, epem_epvebbbdub, real (roots, kind=default), &
  !      (/ mass(11), mass(11), mass(11), mass(12), mass(5), mass(5), mass(1), mass(2) /), &
  !      tolerance = tolerance)
  ! 
  ! stop
  ! 
  ! call check_omega ("e+ e- -> e+ nue b bbar d ubar: Theta vs. Constant", n, &
  !      single_top, single_top_constant, real (roots, kind=default), &
  !      (/ mass(11), mass(11), mass(11), mass(12), mass(5), mass(5), mass(1), mass(2) /), &
  !      tolerance = tolerance)
  ! 
  ! call check_omega ("e+ e- -> e+ nue b bbar d ubar: Theta vs. Fudged", n, &
  !      single_top, single_top_fudged, real (roots, kind=default), &
  !      (/ mass(11), mass(11), mass(11), mass(12), mass(5), mass(5), mass(1), mass(2) /), &
  !      tolerance = tolerance)
  ! 
  ! call check_omega ("e+ e- -> e+ nue b bbar d ubar: Constant vs. Fudged", n, &
  !      single_top_constant, single_top_fudged, real (roots, kind=default), &
  !      (/ mass(11), mass(11), mass(11), mass(12), mass(5), mass(5), mass(1), mass(2) /), &
  !      tolerance = tolerance)

end program maint
