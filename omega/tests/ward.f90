! $Id: ward.f90 3930 2012-09-09 18:48:11Z jr_reuter $
! ward.f90 -- check On Shell Ward Identities in O'Mega 
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

module interface_ward_physical
  use omega_interface
  use amplitude_ward_physical
  implicit none
  private
  public :: load
contains
  pure function load () result (p)
    type(omega_procedures) :: p
    p%number_particles_in => number_particles_in
    p%number_particles_out => number_particles_out
    p%number_spin_states => number_spin_states
    p%spin_states => spin_states
    p%number_flavor_states => number_flavor_states
    p%flavor_states => flavor_states
    p%number_color_indices => number_color_indices
    p%number_color_flows => number_color_flows
    p%color_flows => color_flows
    p%number_color_factors => number_color_factors
    p%color_factors => color_factors
    p%color_sum => color_sum
    p%new_event => new_event
    p%reset_helicity_selection => reset_helicity_selection
    p%is_allowed => is_allowed
    p%get_amplitude => get_amplitude
  end function load
end module interface_ward_physical

module interface_ward_unphysical
  use omega_interface
  use amplitude_ward_unphysical
  implicit none
  private
  public :: load
contains
  pure function load () result (p)
    type(omega_procedures) :: p
    p%number_particles_in => number_particles_in
    p%number_particles_out => number_particles_out
    p%number_spin_states => number_spin_states
    p%spin_states => spin_states
    p%number_flavor_states => number_flavor_states
    p%flavor_states => flavor_states
    p%number_color_indices => number_color_indices
    p%number_color_flows => number_color_flows
    p%color_flows => color_flows
    p%number_color_factors => number_color_factors
    p%color_factors => color_factors
    p%color_sum => color_sum
    p%new_event => new_event
    p%reset_helicity_selection => reset_helicity_selection
    p%is_allowed => is_allowed
    p%get_amplitude => get_amplitude
  end function load
end module interface_ward_unphysical

program ward
  use kinds
  use ward_lib
  use interface_ward_physical, load_physical => load
  use interface_ward_unphysical, load_unphysical => load
  use parameters_ward
  implicit none
  integer, parameter :: N = 1000
  real(kind=default), parameter :: THRESHOLD=0.7
  real(kind=default), parameter :: ROOTS = 1000
  integer, parameter :: SEED = 42
  integer :: failures, attempts
  call init_parameters ()
  call check (load_physical (), load_unphysical (), &
              roots = ROOTS, threshold = THRESHOLD, n = N, seed = SEED, &
              failures = failures, attempts = attempts)
  if (failures .gt. attempts) then
     stop 2
  else if (failures .gt. 0) then
     print *, failures, " failures in ", attempts, " attempts"
     stop 1
  end if
end program ward

