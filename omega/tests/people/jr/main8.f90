! main8.f90 --

program main8
  use kinds
  use tao_random_numbers
  use testbed
  use rambo
  use omega_amplitudes8

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)

  call check_omega ("A A -> F F F F P P", n, oaa_ffffpp, oaa_ffffpp, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default, &
       0.0_default, 0.0_default, 0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ 1, 1, 2, -1, 7, 8, -1, 3, 4, -1, 3, 5, -1, 3, 6, -1, 4, 5, -1, 4, 6, -1, 5, 6  /), (/ 3, 8/)), &  
       states = (/ 1, 1, 2, 2, 2, 2, 2, 2 /), tolerance = tolerance, mode = mode)

end program main8



