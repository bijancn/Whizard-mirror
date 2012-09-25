! $Id: ward_lib.f90 3975 2012-09-25 06:19:40Z kilian $
! ward_lib.f90 -- check On Shell Ward Identities in O'Mega 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ward_lib
  ! use ieee_arithmetic
  use kinds
  use constants
  use tao_random_numbers
  use omega95
  use omega_interface
  use omega_testtools
  implicit none
  private
  public :: check
contains

  elemental function ieee_is_nan (x) result (yorn)
    logical :: yorn
    real (kind=default), intent(in) :: x
    yorn = (x /= x)
  end function ieee_is_nan

  subroutine check (physical, unphysical, roots, threshold, n, failures, attempts, seed)
    type(omega_procedures), intent(in) :: physical, unphysical
    real(kind=default), intent(in) :: roots, threshold
    integer, intent(in) :: n
    integer, intent(out) :: failures, attempts
    integer, intent(in), optional :: seed
    logical :: match, passed
    integer :: n_out, n_flv, n_hel, n_col
    integer :: i, i_flv, i_hel, i_col
    integer :: i_prt
    integer, dimension(:,:), allocatable :: spin_states_phys, spin_states_unphys
    real(kind=default), dimension(:,:), allocatable :: p
    complex(kind=default), dimension(:), allocatable :: a
    character(len=80) :: msg
    complex(kind=default) :: wi
    real(kind=default) :: r, phi, tolerance, a_avg
    integer :: size
    integer, dimension(:), allocatable :: seeds
    failures = 0
    attempts = 0
    call quantum_numbers (physical, unphysical, n_out, n_flv, n_hel, n_col, match)
    if (.not.match) then
       failures = 1
       return
    end if
    if (present (seed)) then
       call tao_random_seed (seed)
    end if
    call physical%reset_helicity_selection (-1.0_default, -1)
    call unphysical%reset_helicity_selection (-1.0_default, -1)
    allocate (p(0:3,2+n_out))
    allocate (a(n_hel))
    allocate (spin_states_phys(2+n_out,n_hel))
    allocate (spin_states_unphys(2+n_out,unphysical%number_spin_states()))
    call physical%spin_states(spin_states_phys)
    call unphysical%spin_states(spin_states_unphys)
    call beams (ROOTS, 0.0_default, 0.0_default, p(:,1), p(:,2))
    do i = 1, N
       call massless_isotropic_decay (ROOTS, p(:,3:))
       call physical%new_event (p)
       call unphysical%new_event (p)
       do i_flv = 1, n_flv
          do i_col = 1, n_col
             do i_hel = 1, n_hel
                a(i_hel) = physical%get_amplitude (i_flv, i_hel, i_col)

!                do i_prt = 1, (2+n_out)
!                   if (spin_states_phys(i_prt,i_hel).eq.0) then
!                      a(i_hel) = 0.0_default
!                      exit
!                   end if
!                end do

             end do
             a_avg = sum (abs (a)) / n_hel

!write (*, "(1X,'a_avg=',E15.5)") a_avg

             if (.not. ieee_is_nan (a_avg)) then
                if (a_avg > 0) then
                   do i_hel = 1, n_hel / 2
!                   do i_hel = 1, size(spin_states_unphys,dim=2)
                      wi = unphysical%get_amplitude (i_flv, i_hel, i_col)

!                      do i_prt = 1, (2+n_out)
!                         if (spin_states_unphys(i_prt,i_hel).eq.0) then
!                            wi = 0.0_default
!                            exit
!                         end if
!                      end do

                      attempts = attempts + 1
                      write (msg, "(1X,'evt=',I5,', flv=',I3,', col=',I3,', hel=',I3)") &
                           i, i_flv, i_col, i_hel
                      passed = .true.
                      call expect_zero (wi, a_avg, trim(msg), passed, &
                           quiet=.true., threshold = threshold)
                      if (.not.passed) then
                         failures = failures + 1
                      end if
                   end do
                else
                   ! write (*, "(1X,'evt=',I5,', flv=',I3,', col=',I3,': ', A)") &
                   !      i, i_flv, i_col, "skipped: physical amplitude vanishes"
                end if
             else
                write (*, "(1X,'evt=',I5,', flv=',I3,', col=',I3,': ', A)") &
                     i, i_flv, i_col, "physical amplitude NaN"
                attempts = attempts + 1
                failures = failures + 1
             end if
          end do
       end do
    end do
    deallocate (p)
    deallocate (a)
    deallocate (spin_states_phys)
    deallocate (spin_states_unphys)
  end subroutine check

  subroutine quantum_numbers (physical, unphysical, n_out, n_flv, n_hel, n_col, match)
    type(omega_procedures), intent(in) :: physical, unphysical
    integer, intent(out) :: n_out, n_flv, n_hel, n_col
    logical, intent(out) :: match
    integer, dimension(:,:), allocatable :: &
         physical_flavor_states, unphysical_flavor_states, &
         physical_spin_states, unphysical_spin_states
    integer, dimension(:,:,:), allocatable :: &
         physical_color_flows, unphysical_color_flows
    logical, dimension(:,:), allocatable :: &
         physical_ghost_flags, unphysical_ghost_flags
    type(omega_color_factor), dimension(:), allocatable :: &
         physical_color_factors, unphysical_color_factors
    integer :: n_in, n_prt, n_cix, n_cfs
    n_in = physical%number_particles_in ()
    n_out = physical%number_particles_out ()
    n_prt = n_in + n_out
    n_flv = physical%number_flavor_states ()
    n_hel = physical%number_spin_states ()
    n_cix = physical%number_color_indices ()
    n_col = physical%number_color_flows ()
    n_cfs = physical%number_color_factors ()
    match = .true.
    if (unphysical%number_particles_in () .ne. n_in) then
       print *, "#particles_in don't match!"
       match = .false.
    end if
    if (unphysical%number_particles_out () .ne. n_out) then
       print *, "#particles_out don't match!"
       match = .false.
    end if
    if (unphysical%number_flavor_states () .ne. n_flv) then
       print *, "#flavor_states don't match!"
       match = .false.
    end if
    if (unphysical%number_spin_states () .ne. n_hel/2) then
       print *, "#spin_states don't match!"
!       match = .false.
    end if
    if (unphysical%number_color_indices () .ne. n_cix) then
       print *, "#color_indices don't match!"
       match = .false.
    end if
    if (unphysical%number_color_flows () .ne. n_col) then
       print *, "#color_flows don't match!"
       match = .false.
    end if
    if (unphysical%number_color_factors () .ne. n_cfs) then
       print *, "#color_factors don't match!"
       match = .false.
    end if
    if (match) then
       allocate (physical_flavor_states(n_prt,n_flv), unphysical_flavor_states(n_prt,n_flv))
       allocate (physical_spin_states(n_prt,n_hel), unphysical_spin_states(n_prt,n_hel/2))
       allocate (physical_color_flows(n_cix,n_prt,n_col), &
                 unphysical_color_flows(n_cix,n_prt,n_col))
       allocate (physical_ghost_flags(n_prt,n_col), unphysical_ghost_flags(n_prt,n_col))
       allocate (physical_color_factors(n_cfs), unphysical_color_factors(n_cfs))
       call physical%flavor_states (physical_flavor_states)
       call unphysical%flavor_states (unphysical_flavor_states)
       call physical%spin_states (physical_spin_states)
       call unphysical%spin_states (unphysical_spin_states)
       call physical%color_flows (physical_color_flows, physical_ghost_flags)
       call unphysical%color_flows (unphysical_color_flows, unphysical_ghost_flags)
       call physical%color_factors (physical_color_factors)
       call unphysical%color_factors (unphysical_color_factors)
       if (any (physical_flavor_states .ne. unphysical_flavor_states)) then
          print *, "flavor states don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       ! if (any (physical_spin_states .ne. unphysical_spin_states)) then
       !    print *, "spin states don't match!"
       !    print *, "CAVEAT: this might be due to simple reordering!"
       !    match = .false.
       ! end if
       if (any (physical_color_flows .ne. unphysical_color_flows)) then
          print *, "color flows don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       if (any (physical_ghost_flags .neqv. unphysical_ghost_flags)) then
          print *, "ghost flags don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       if (any (.not. color_factors_equal (physical_color_factors, &
                                           unphysical_color_factors))) then
          print *, "color_factors don't match!"
          print *, "CAVEAT: this might be due to simple reordering!"
          match = .false.
       end if
       deallocate (physical_flavor_states, unphysical_flavor_states)
       deallocate (physical_spin_states, unphysical_spin_states)
       deallocate (physical_color_flows, unphysical_color_flows)
       deallocate (physical_ghost_flags, unphysical_ghost_flags)
       deallocate (physical_color_factors, unphysical_color_factors)
    end if
  end subroutine quantum_numbers

  elemental function color_factors_equal (cf1, cf2) result (eq)
    logical :: eq
    type(omega_color_factor), intent(in) :: cf1, cf2
    eq = (cf1%i1 .eq. cf2%i1) .and. (cf1%i2 .eq. cf2%i2) .and. (cf1%factor .eq. cf2%factor)
  end function color_factors_equal

  pure function dot (p, q) result (pq)
    real(kind=default), dimension(0:), intent(in) :: p, q
    real(kind=default) :: pq
    pq = p(0)*q(0) - dot_product (p(1:), q(1:))
  end function dot

  pure function mass2 (p) result (m2)
    real(kind=default), dimension(0:), intent(in) :: p
    real(kind=default) :: m2
    m2 = p(0)*p(0) - p(1)*p(1) - p(2)*p(2) - p(3)*p(3)
  end function mass2

  pure subroutine beams (roots, m1, m2, p1, p2)
    real(kind=default), intent(in) :: roots, m1, m2
    real(kind=default), dimension(0:), intent(out) :: p1, p2
    real(kind=default) :: m12, m22
    m12 = m1**2
    m22 = m2**2
    p1(0) = (roots**2 + m12 - m22) / (2*roots)
    p1(1:2) = 0
    p1(3) = sqrt (p1(0)**2 - m12)
    p2(0) = roots - p1(0)
    p2(1:3) = - p1(1:3)
  end subroutine beams

  ! The massless RAMBO algorithm
  subroutine massless_isotropic_decay (roots, p)
    real(kind=default), intent(in) :: roots
    real(kind=default), dimension(0:,:), intent(out) :: p
    real(kind=default), dimension(0:3,size(p,dim=2)) :: q
    real(kind=default), dimension(0:3) :: qsum
    real(kind=double), dimension(4) :: ran_double
    real(kind=default), dimension(4) :: ran
    real(kind=default) :: c, s, f, qabs, x, r, z
    integer :: k
    ! Generate isotropic null vectors
    do k = 1, size (p, dim = 2)
       ! if default is not double or single, we can't use
       ! tao_random_number directly ...
       call tao_random_number (ran_double)
       ran = ran_double
       ! generate a x*exp(-x) distribution for q(0,k)
       q(0,k)= -log(ran(1)*ran(2))
       c = 2*ran(3)-1
       f = 2*PI*ran(4)
       s = sqrt(1-c*c)
       q(2,k) = q(0,k)*s*sin(f)  
       q(3,k) = q(0,k)*s*cos(f)
       q(1,k) = q(0,k)*c
    enddo
    ! Boost and rescale the vectors
    qsum = sum (q, dim = 2)
    qabs = sqrt (dot (qsum, qsum))
    x = roots/qabs
    do k = 1, size (p, dim = 2)
       r = dot (q(0:,k), qsum) / qabs
       z = (q(0,k)+r)/(qsum(0)+qabs)
       p(1:3,k) = x*(q(1:3,k)-qsum(1:3)*z)
       p(0,k) = x*r
    enddo
  end subroutine massless_isotropic_decay

end module ward_lib
