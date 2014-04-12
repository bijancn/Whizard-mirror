! $Id: compare.f90 4926 2013-12-04 12:35:06Z jr_reuter $
! compare.f90 -- compare amplitudes created by two versions of O'Mega 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
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

module compare_lib

  use kinds
  use constants
  use omega95

  use parameters_QCD

  use amplitude_compare_v1, &
       v1_number_particles_in => number_particles_in, &
       v1_number_particles_out => number_particles_out, &
       v1_number_spin_states => number_spin_states, &
       v1_spin_states => spin_states, &
       v1_number_flavor_states => number_flavor_states, &
       v1_flavor_states => flavor_states, &
       v1_number_color_flows => number_color_flows, &
       v1_color_flows => color_flows, &
       v1_number_color_indices => number_color_indices, &
       v1_number_color_factors => number_color_factors, &
       v1_color_factors => color_factors, &
       v1_color_sum => color_sum, &
       v1_reset_helicity_selection => reset_helicity_selection, &
       v1_new_event => new_event, &
       v1_is_allowed => is_allowed, &
       v1_get_amplitude => get_amplitude

  use amplitude_compare_v2, &
       v2_number_particles_in => number_particles_in, &
       v2_number_particles_out => number_particles_out, &
       v2_number_spin_states => number_spin_states, &
       v2_spin_states => spin_states, &
       v2_number_flavor_states => number_flavor_states, &
       v2_flavor_states => flavor_states, &
       v2_number_color_flows => number_color_flows, &
       v2_color_flows => color_flows, &
       v2_number_color_indices => number_color_indices, &
       v2_number_color_factors => number_color_factors, &
       v2_color_factors => color_factors, &
       v2_color_sum => color_sum, &
       v2_reset_helicity_selection => reset_helicity_selection, &
       v2_new_event => new_event, &
       v2_is_allowed => is_allowed, &
       v2_get_amplitude => get_amplitude

  implicit none

  contains

    subroutine quantum_numbers (match, n_out, n_flv, n_hel, n_col)
      logical, intent(out) :: match
      integer, intent(out) :: n_out, n_flv, n_hel, n_col
      integer, dimension(v1_number_particles_in()+v1_number_particles_out(), &
                         v1_number_flavor_states()) :: v1_table_flavor_states
      integer, dimension(v2_number_particles_in()+v2_number_particles_out(), &
                         v2_number_flavor_states()) :: v2_table_flavor_states
      integer, dimension(v1_number_particles_in()+v1_number_particles_out(), &
                         v1_number_spin_states()) :: v1_table_spin_states
      integer, dimension(v2_number_particles_in()+v2_number_particles_out(), &
                         v2_number_spin_states()) :: v2_table_spin_states
      integer, dimension(v1_number_color_indices(), &
                         v1_number_particles_in()+v1_number_particles_out(), &
                         v1_number_color_flows()) :: v1_table_color_flows
      integer, dimension(v2_number_color_indices(), &
                         v2_number_particles_in()+v2_number_particles_out(), &
                         v2_number_color_flows()) :: v2_table_color_flows
      logical, dimension(v1_number_particles_in()+v1_number_particles_out(), &
                         v1_number_color_flows()) :: v1_table_ghost_flags
      logical, dimension(v2_number_particles_in()+v2_number_particles_out(), &
                         v2_number_color_flows()) :: v2_table_ghost_flags
      type(omega_color_factor), dimension(v1_number_color_factors()) :: v1_table_color_factors
      type(omega_color_factor), dimension(v2_number_color_factors()) :: v2_table_color_factors
      match = .true.
      n_out = v1_number_particles_out ()
      n_flv = v1_number_flavor_states ()
      n_hel = v1_number_spin_states ()
      n_col = v1_number_color_flows ()
      call v1_flavor_states (v1_table_flavor_states)
      call v2_flavor_states (v2_table_flavor_states)
      call v1_spin_states (v1_table_spin_states)
      call v2_spin_states (v2_table_spin_states)
      call v1_color_flows (v1_table_color_flows, v1_table_ghost_flags)
      call v2_color_flows (v2_table_color_flows, v2_table_ghost_flags)
      call v1_color_factors (v1_table_color_factors)
      call v2_color_factors (v2_table_color_factors)
      if (     size (v1_table_flavor_states, dim=2) &
          .ne. size (v2_table_flavor_states, dim=2)) then
         match = .false.
         print *, "#flavor_states don't match!"
      else if (any (v1_table_flavor_states .ne. v2_table_flavor_states)) then
         match = .false.
         print *, "flavor states don't match!"
         print *, "CAVEAT: this might be due to simple reordering!"
      end if
      if (     size (v1_table_spin_states, dim=2) &
          .ne. size (v2_table_spin_states, dim=2)) then
         match = .false.
         print *, "#spin_states don't match!"
      else if (any (v1_table_spin_states .ne. v2_table_spin_states)) then
         match = .false.
         print *, "spin states don't match!"
         print *, "CAVEAT: this might be due to simple reordering!"
      end if
      if (     size (v1_table_color_flows, dim=3) &
          .ne. size (v2_table_color_flows, dim=3)) then
         match = .false.
         print *, "#color_flows don't match!"
      else if (any (v1_table_color_flows .ne. v2_table_color_flows)) then
         match = .false.
         print *, "color flows don't match!"
         print *, "CAVEAT: this might be due to simple reordering!"
      else if (any (v1_table_ghost_flags .neqv. v2_table_ghost_flags)) then
         match = .false.
         print *, "ghost flags don't match!"
         print *, "CAVEAT: this might be due to simple reordering!"
      end if
      if (     size (v1_table_color_factors) &
          .ne. size (v2_table_color_factors)) then
         match = .false.
         print *, "#color_factors don't match!"
      else if (any (.not. color_factors_equal (v1_table_color_factors, &
                                               v2_table_color_factors))) then
         match = .false.
         print *, "color factors don't match!"
         print *, "CAVEAT: this might be due to simple reordering!"
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
      real(kind=default), dimension(4) :: ran
      real(kind=default) :: c, s, f, qabs, x, r, z
      integer :: k
      ! Generate isotropic null vectors
      do k = 1, size (p, dim = 2)
         call random_number (ran)
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

    subroutine expect (x, y, tolerance)
      real(kind=default), intent(in) :: x, y
      integer, intent(in) :: tolerance
      if (abs (x - y) .gt. tolerance * epsilon (max (x, y))) then
         stop 1
      end if
    end subroutine expect

end module compare_lib

program compare
  use kinds
  use compare_lib
  use parameters_QCD
  implicit none
  logical :: match
  real(kind=default), parameter :: ROOTS = 1000
  real(kind=default), parameter :: SCALE = 100
  integer, parameter :: N = 10
  integer :: n_out, n_flv, n_hel, n_col
  integer :: i, i_flv, i_hel, i_col, failed_moduli, failed_phases
  integer :: is1, is2, is3, is4, is5, is6
  real(kind=default), dimension(:,:), allocatable :: p
  complex(kind=default), dimension(:), allocatable :: a1, a2
  real(kind=default) :: r1, r2, phi1, phi2, tolerance
  integer :: size
  integer, dimension(:), allocatable :: seed
  character(len=*), parameter :: fmt_pfx = &
       "(1X,'evt=',I4,', flv=',I3,', col=',I3,', hel=',I3,', ',"
  call random_seed (size = size)
  allocate (seed(size))
  seed = 42
  call random_seed (put = seed)
  deallocate (seed)
  call init_parameters
  call v1_reset_helicity_selection (-1.0_default, -1)
  call v2_reset_helicity_selection (-1.0_default, -1)
  call quantum_numbers (match, n_out, n_flv, n_hel, n_col)
  if (match) then
     allocate (p(0:3,2+n_out))
     allocate (a1(n_hel), a2(n_hel))
     call beams (ROOTS, 0.0_default, 0.0_default, p(:,1), p(:,2))
     failed_moduli = 0
     failed_phases = 0
     do i = 1, N
        call massless_isotropic_decay (ROOTS, p(:,3:))
        call v1_new_event (p)
        call v2_new_event (p)
        do i_flv = 1, n_flv
           do i_col = 1, n_col
              do i_hel = 1, n_hel
                 a1(i_hel) = v1_get_amplitude (i_flv, i_hel, i_col)
                 a2(i_hel) = v2_get_amplitude (i_flv, i_hel, i_col)
              end do
              tolerance = SCALE * epsilon (SCALE) &
                   * ((sum (abs (a1)) + sum (abs (a2))) / n_hel)
              do i_hel = 1, n_hel
                 if (abs (a1(i_hel) - a2(i_hel)) .gt. tolerance) then
                    r1 = abs (a1(i_hel))
                    r2 = abs (a2(i_hel))
                    if (abs (r1 - r2) .gt. tolerance) then
                       write (unit = *, &
                              fmt = fmt_pfx // "'moduli= ',E10.4,', ',E10.4)") &
                            i, i_flv, i_col, i_hel, r1, r2
                       failed_moduli = failed_moduli + 1
                    else
                       phi1 = atan2 (real (a1(i_hel)), imag (a1(i_hel)))
                       phi2 = atan2 (real (a2(i_hel)), imag (a2(i_hel)))
                       write (unit = *, &
                              fmt = fmt_pfx // "'phases= ',F10.4,', ',F10.4)") &
                            i, i_flv, i_col, i_hel, phi1, phi2
                       failed_phases = failed_phases + 1
                    end if
                 end if
              end do
           end do
        end do
     end do
     deallocate (p)
     deallocate (a1, a2)
     print *, failed_moduli + failed_phases, " failures (", &
          failed_moduli, " moduli, ", failed_phases, " phases) in ", &
          N * n_flv * n_hel * i_col, " attempts"
     if (failed_moduli .gt. 0) then
        stop 2
     else if (failed_phases .gt. 0) then
        stop 1
     end if
  else
     stop 3
  end if
end program compare

