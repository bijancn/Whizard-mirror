! WHIZARD <<Version>> <<Date>>
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com>
!     Sebastian Schmidt
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
!! Dummy interface for non-existent LCIO library
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Tell the caller that this is not the true LCIO library
     logical(c_bool) function lcio_available () bind(C)
       use iso_c_binding
       lcio_available = .false.
     end function lcio_available

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! LCEventImpl functions

! extern "C" void* new_lcevent( int proc_id, int event_id ) {}
     type(c_ptr) function new_lcevent (proc_id, event_id) bind(C)
       use iso_c_binding
       integer(c_int), value :: proc_id, event_id
       new_lcevent = c_null_ptr
     end function new_lcevent

! extern "C" void lcevent_delete( void* evt) {}
     subroutine lcevent_delete (evt_obj) bind(C)
       use iso_c_binding
       type(c_ptr), value :: evt_obj
     end subroutine lcevent_delete

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MCParticleImpl functions

! extern "C" MCParticleImpl* new_lcio_particle(void* momentum, int pdg_id, int status)
     type(c_ptr) function new_lcio_particle (mom, pdg_id, mass, status) bind(C)
       use iso_c_binding
       integer(c_int), value :: pdg_id, status
       real(c_double), dimension(3) :: mom
       real(c_double), value :: mass
       new_lcio_particle = c_null_ptr
     end function new_lcio_particle

! extern "C" MCParticleImpl* lcio_set_color_flow
     subroutine lcio_set_color_flow (evt_obj, cflow) bind (C)
       use iso_c_binding
       type(c_ptr), value :: evt_obj
       integer(c_int), dimension(2) :: cflow
     end subroutine lcio_set_color_flow

! extern "C" const int* lcio_particle_get_flow
     subroutine lcio_particle_get_flow (evt_obj, cflow) bind (C)
       use iso_c_binding
       type(c_ptr), value :: evt_obj
       integer(c_int), dimension(2) :: cflow
     end subroutine lcio_particle_get_flow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! LCRunHeader functions

! extern "C" void* runHdr()
  type(c_ptr) function runHdr () bind(C)
    use iso_c_binding
    runHdr = c_null_ptr
  end function runHdr

