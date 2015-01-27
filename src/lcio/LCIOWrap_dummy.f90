! WHIZARD <<Version>> <<Date>>
! 
! Copyright (C) 1999-2015 by 
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

! extern "C" void* new_lcio_event( int proc_id, int event_id ) {}
     type(c_ptr) function new_lcio_event (proc_id, event_id) bind(C)
       use iso_c_binding
       integer(c_int), value :: proc_id, event_id
       new_lcio_event = c_null_ptr
     end function new_lcio_event

! extern "C" void lcio_event_delete( void* evt) {}
     subroutine lcio_event_delete (evt_obj) bind(C)
       use iso_c_binding
       type(c_ptr), value :: evt_obj
     end subroutine lcio_event_delete

! extern "C" void lcio_event_add_collection ( LCEventImpl* evt, LCCollectionVec* mcVec )     
     subroutine lcio_event_add_collection (evt_obj, lccoll_obj) bind(C)
       use iso_c_binding
       type(c_ptr), value :: evt_obj, lccoll_obj
     end subroutine lcio_event_add_collection
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MCParticleImpl and LCCollectionVec functions

! extern "C" LCCollectionVec* new_lccollection()
     type(c_ptr) function new_lccollection () bind (C)
       use iso_c_binding
       new_lccollection = c_null_ptr
     end function new_lccollection

! extern "C" void add_particle_to_collection (MCParticleImpl* mcp, LCCollectionVec* mcVec)
     subroutine add_particle_to_collection (prt_obj, lccoll_obj) bind (C)
       use iso_c_binding
       type(c_ptr), value :: prt_obj, lccoll_obj
     end subroutine add_particle_to_collection
     
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
!! LCWriter functions

! extern "C" LCWriter* open_lcio_writer_new 
     type (c_ptr) function open_lcio_writer_new (filename, complevel) bind (C)
       use iso_c_binding
       character(c_char), dimension(*), intent(in) :: filename
       integer(c_int), value :: complevel
       open_lcio_writer_new = c_null_ptr
     end function open_lcio_writer_new
     
! extern "C" LCWriter* lcio_writer_delete
     subroutine lcio_writer_delete (io_obj) bind (C)
       use iso_c_binding
       type(c_ptr), value :: io_obj
     end subroutine lcio_writer_delete

! extern "C" LCWriter* lcio_write_event
     subroutine lcio_write_event (io_obj, evt_obj) bind (C)
       use iso_c_binding
       type(c_ptr), value :: io_obj, evt_obj
     end subroutine lcio_write_event

! extern "C" void lcio_particle_add_parent
     subroutine lcio_particle_add_parent (io_obj1, io_obj2) bind (C)
       use iso_c_binding
       type(c_ptr), value :: io_obj1, io_obj2
     end subroutine lcio_particle_add_parent
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! LCRunHeader functions

! extern "C" LCRunHeaderImpl* new_lcio_run_header( int run_id ) {
     type(c_ptr) function new_lcio_run_header (run_id) bind(C)
       use iso_c_binding
       integer(c_int), value :: run_id
       new_lcio_run_header = c_null_ptr
     end function new_lcio_run_header

! extern "C" void run_header_set_simstring (LCRunHeaderImpl* runHdr, char* simstring)      
     subroutine run_header_set_simstring (runhdr_obj, simstring) bind (C)
       use iso_c_binding
       type(c_ptr), value :: runhdr_obj
       character(c_char), dimension(*), intent(in) :: simstring
     end subroutine run_header_set_simstring

! extern "C" void write_run_header (LCWriter* lcWrt, const LCRunHeaderImpl* runHdr)
     subroutine write_run_header (lcwrt_obj, runhdr_obj) bind (C)
       use iso_c_binding
       type(c_ptr), value :: lcwrt_obj, runhdr_obj
     end subroutine write_run_header
