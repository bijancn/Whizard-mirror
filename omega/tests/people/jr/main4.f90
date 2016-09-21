! $Id: main4.f90 1203 2009-10-08 11:39:11Z jr_reuter $

program main4
  use kinds
  use tao_random_numbers
  use testbed
  use rambo
  use omega_amplitudes4

  real(kind=single) :: roots
  integer :: n, tolerance
  character (len=8) :: mode

  call setup_parameters ()
  call read_parameters (roots, n, tolerance, mode)

  call check_omega ("A A -> F F", n, oaa_ff, oaa_ff, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ -1, 3, 4, 1, 1, 2 /), (/ 3, 2/)), &  
       states = (/ 1, 1, 2, 2 /), tolerance = tolerance, mode = mode) 

  call check_omega ("F F -> A A", n, off_aa, off_aa, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default /), & 
       symmetry = reshape ((/ 1, 3, 4, -1, 1, 2 /), (/ 3, 2/)), &  
       states = (/ 2, 2, 1, 1 /), tolerance = tolerance, mode = mode) 

  call check_omega ("F A -> F A", n, ofa_fa, ofa_fa, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default/), &
       symmetry = reshape ((/ 1, 1, 3 /), (/3, 1/)), &
       states = (/ 2, 1, 2, 1 /), tolerance = tolerance, mode = mode)   

  call check_omega ("F F -> F F", n, off_ff, off_ff, &
       real (roots, kind=default), &
       (/ 0.0_default, 0.0_default, 0.0_default, 0.0_default /), &
       symmetry = reshape ((/ -1, 1, 2, -1, 3, 4/), (/3, 2/)), &
       states = (/ 2, 2, 2, 2 /), tolerance = tolerance, mode = mode)

end program main4


