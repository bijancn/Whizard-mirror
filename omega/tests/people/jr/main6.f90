! main6.f90 --

program main6
  use kinds
  use tao_random_numbers
  use testbed
  use rambo
  use omega_amplitudes6

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)

  call check_omega ("A A -> F F F F", n, oaa_ffff, oaa_ffff, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default, &
       0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ 1, 1, 2, -1, 3, 4, -1, 3, 5, -1, 3, 6, -1, 4, 5, -1, 4, 6, -1, 5, 6 /), (/ 3, 7/)), &  
       states = (/ 1, 1, 2, 2, 2, 2 /), tolerance = tolerance, mode = mode)

  call check_omega ("A A -> P P P P", n, oaa_pppp, oaa_pppp, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default, &
       0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ 1, 1, 2, -1, 3, 4, -1, 3, 5, -1, 3, 6, -1, 4, 5, -1, 4, 6, -1, 5, 6 /), (/ 3, 7/)), &  
       states = (/ 1, 1, 2, 2, 2, 2 /), tolerance = tolerance, mode = mode)

  call check_omega ("A A -> F F P P", n, oaa_ffpp, oaa_ffpp, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default, &
       0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ 1, 1, 2, -1, 3, 4, -1, 5, 6 /), (/ 3, 3/)), &  
       states = (/ 1, 1, 2, 2, 2, 2 /), tolerance = tolerance, mode = mode)

  call check_omega ("F F -> P P A A", n, off_ppaa, off_ppaa, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default, &
       0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ 1, 5, 6, -1, 1, 2, -1, 3, 4 /), (/ 3, 3/)), &  
       states = (/ 2, 2, 2, 2, 1, 1 /), tolerance = tolerance, mode = mode)

end program main6



