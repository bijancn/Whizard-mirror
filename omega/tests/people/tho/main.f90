! $Id: main.f90 1203 2009-10-08 11:39:11Z jr_reuter $

program main
  use kinds
  use tao_random_numbers
  use testbed
  ! use omega_parameters1, only: setup_parameters1 => setup_parameters
  ! use omega_parameters2, only: setup_parameters2 => setup_parameters
  use omega_parameters
  implicit none
  real(kind=single) :: roots
  real(kind=default), dimension(5) :: fudge
  integer :: n, seed, tolerance
  character (len=8) :: mode
  call read_parameters (roots, n, tolerance, mode)
  !!! (Very) old version
  !!! call read_parameters (roots, n, seed, tolerance)
  call tao_random_seed (seed)
  call tao_random_number (alpha4)
  call tao_random_number (alpha5)
  call tao_random_number (fudge)
  call setup_parameters ()
  ! call setup_parameters1 ()
  ! call setup_parameters2 ()
  alww0 = alww0 * fudge(1)
  alww2 = alww2 * fudge(2)
  alzw1 = alzw1 * fudge(3)
  alzw0 = alzw0 * fudge(4)
  alzz = alzz * fudge(5)
  ialww0 = ialww0 * sqrt (fudge(1))
  ialww2 = ialww2 * sqrt (fudge(2))
  ialzw1 = ialzw1 * sqrt (fudge(3))
  ialzw0 = ialzw0 * sqrt (fudge(4))
  ialzz = ialzz * sqrt (fudge(5))
  call check ("W+ W- -> W+ W-", n, real (roots, kind=default), &
       (/ 24, -24, 24, -24 /), (/ mass(24), mass(24), mass(24), mass(24) /), &
       tolerance = tolerance)
  call check ("W+ W- -> Z Z", n, real (roots, kind=default), &
       (/ 24, -24, 23, 23 /), (/ mass(24), mass(24), mass(23), mass(23) /), &
       symmetry = reshape ( (/ 1, 3, 4 /), (/ 3, 1 /) ), tolerance = tolerance)
  call check ("Z Z -> Z Z", n, real (roots, kind=default), &
       (/ 23, 23, 23, 23 /), (/ mass(23), mass(23), mass(23), mass(23) /), &
       symmetry = reshape ( (/ 1, 3, 4 /), (/ 3, 1 /) ), tolerance = tolerance)
contains
  subroutine check (tag, n, roots, flavors, masses, symmetry, tolerance)
    use omega_amplitudes1, only: &
         omega1 => omega_amplitudes1_func
!!!         omega1 => amplitude, &
!!!         omega1_sum => spin_sum_sqme, &
!!!         spin_states1 => spin_states, &
!!!         n_spin_states1 => number_spin_states, &
!!!         n_spin_states_in1 => number_spin_states_in
    use omega_amplitudes2, only: &
         omega2 => omega_amplitudes2_func
!!!         omega2 => amplitude, &
!!!         omega2_sum => spin_sum_sqme, &
!!!         spin_states2 => spin_states, &
!!!         n_spin_states2 => number_spin_states, &
!!!         n_spin_states_in2 => number_spin_states_in
    character(len=*), intent(in) :: tag
    integer, intent(in) :: n
    real(kind=default), intent(in) :: roots
    integer, dimension(:), intent(in) :: flavors
    real(kind=default), dimension(:), intent(in) :: masses
    integer, dimension(0:,:), intent(in), optional :: symmetry
    integer, intent(in), optional :: tolerance
    call check_omega (tag, n, omega1, omega2, &
         roots, masses, symmetry, flavors, tolerance, mode)
    !!! (Very) old version
    !!! call check_omega (tag, n, roots, flavors, masses, &
    !!!      omega1, omega1_sum, spin_states1, n_spin_states1, n_spin_states_in1, &
    !!!      omega2, omega2_sum, spin_states2, n_spin_states2, n_spin_states_in2, &
    !!!      symmetry, tolerance)
  end subroutine check
end program main

