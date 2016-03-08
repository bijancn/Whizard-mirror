! WHIZARD 1.13 Aug 06 2001
! 
! (C) 1999-2001 by Wolfgang Kilian <kilian@particle.uni-karlsruhe.de>
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
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

! For the LEP Monte Carlos, a standard common block has been proposed
! in AKV89.  We strongly recommend its use.  (The description is an
! abbreviated transcription of AKV89, Vol. 3, pp. 327-330).
!
!
! NMXHEP is the maximum number of entries:
!
!
! NEVHEP is normally the event number, but may take special
! values as follows:
!
!    0   the program does not keep track of event numbers.
!   -1   a special initialization record.
!   -2   a special final record.
!
!
! NHEP holds the number of entries for this event.
!
!
! The entry ISTHEP(N) gives the status code for the Nth entry,
! with the following semantics:
!    0       a null entry.
!    1       an existing entry, which has not decayed or fragmented.
!    2       a decayed or fragmented entry, which is retained for
!            event history information.
!    3       documentation line.
!    4- 10   reserved for future standards.
!   11-200   at the disposal of each model builder.
!  201-      at the disposal of users.
!
!
! The Particle Data Group has proposed standard particle codes,
! which are to be stored in IDHEP(N).
!
!
! JMOHEP(1,N) points to the Nth entry's mother, if any.
! It is set to zero for initial entries.
! JMOHEP(2,N) points to the second mother, if any.
!
!
! JDAHEP(1,N) and JDAHEP(2,N) point to the Nth entry's first and
! last daughter, if any.  These are zero for entries which have not
! yet decayed.  The other daughters are stored in between these two.
!
!
! In PHEP we store the momentum of the particle, more specifically
! this means that PHEP(1,N), PHEP(2,N), and PHEP(3,N) contain the
! momentum in the x, y, and z direction (as defined by the machine
! people), measured in GeV/c.  PHEP(4,N) contains the energy in GeV
! and PHEP(5,N) the mass in GeV/c**2.  The latter may be negative for
! spacelike partons.
!
!
! Finally VHEP is the place to store the position of the production
! vertex.  VHEP(1,N), VHEP(2,N), and VHEP(3,N) contain the x, y,
! and z coordinate (as defined by the machine people), measured in mm.
! VHEP(4,N) contains the production time in mm/c.
!
!
! -------------------------------------------------------------
! hepev4 holds generator level information
!
! idruplh            : The identity of the current process, 
!                      as given by the LPRUP codes.
! eventweightlh      : The event weight:  
!                      Equal to (total cross section)/(total generated)
!                      for the output of Pythia, Herwig, etc.
! alphaqedlh         : QED coupling alpha_em.
! alphaqcdlh         : QCD coupling alpha_s.
! scalelh(10)        : Squared Scale Q of the event.
!......Defined for standard 2->1->2 or 2->2 process
!.......kinematics are p1 + p2 -> q1 + q2
!      scalehl(1)= Q2 hard scale (used in PDF and couplings)
!      scalehl(2)= Q2 scale of parton shower
!      scalehl(3)= s-hat, invariant (p1+p2)**2
!      scalehl(4)= t-hat, invariant (p1-q1)**2
!      scalehl(5)= u-hat, invariant (p1-q2)**2
!      scalehl(6)= squared transverse momentum of q1 (i.e., pt-hat**2)
!.......Additionally, for 2->3 processes, p1 + p2 -> q1 + q2 + q3
!      scalehl(7)= squared transverse momentum of q2
!      scalehl(8)= user defined, 0 by default
!      scalehl(9)= user defined, 0 by default
!      scalehl(10)= user defined, 0 by default
! spinlh(3,..)       : spin information
! icolorflowlh(2,..) : (Anti-)Colour flow.
!

module hepevt_include_common

  !use kinds, only: double !NODEP!
  use hepeup_include_common
!   use diagnostics, only: msg_level
  implicit none

  public

  integer, parameter :: nmxhep = 4000
  integer, dimension(nmxhep) :: ialhep
  integer :: nhide
  real, dimension(nmxhep) :: p3hide
  !real(kind=double), dimension(nmxhep) :: p3hide
  integer :: nevhep, nhep
  integer, dimension(nmxhep) :: isthep, idhep
  integer, dimension(2, nmxhep) :: jmohep, jdahep
  !real(kind=double), dimension(5, nmxhep) :: phep
  !real(kind=double), dimension(4, nmxhep) :: vhep
  real, dimension(5, nmxhep) :: phep
  real, dimension(4, nmxhep) :: vhep
  common /HEPEVT/ nevhep, nhep, isthep, idhep, &
       & jmohep, jdahep, phep, vhep

  real :: eventweightlh, alphaqedlh, alphaqcdlh
  real, dimension(10) :: scalelh
  real, dimension(3, nmxhep) :: spinlh
  !real(kind=double) eventweightlh, alphaqedlh, alphaqcdlh
  !real(kind=double), dimension(10) :: scalelh
  !real(kind=double), dimension(3, nmxhep) :: spinlh
  integer, dimension(2, nmxhep) :: icolorflowlh
  integer :: idruplh

  common/hepev4/eventweightlh, alphaqedlh, alphaqcdlh, scalelh, &
       & spinlh, icolorflowlh, idruplh

  integer :: nhep_original

  integer :: nhepev4_call=0

contains

  subroutine hepev4_fill

    integer :: i
    integer :: j
    !real(kind=double)  :: sumdiff
    real  :: sumdiff

    nhepev4_call=nhepev4_call+1

    spinlh(:,1:nhep_original)=0.
    icolorflowlh(:,1:nhep_original)=0.

    loop_i: do i=1,nhep_original
       loop_j: do j=1,nup
          check_consist: if(idup(j).eq.idhep(i)) then
             sumdiff=sum((pup(1:3,j)-phep(1:3,i))**2)
!             if(nhepev4_call.le.20 .and. msg_level > 2 ) print *, " idup(j),idhep(i),sumdiff= ", idup(j),idhep(i),sumdiff
             check_sumdiff: if(sumdiff.lt.1.d-6) then
                spinlh(3,i)=spinup(j)
                icolorflowlh(:,i)=icolup(:,j)
!                check_nhepev4: if(nhepev4_call.le.20 .and. msg_level > 2 ) then 
!                   print *, " i,idhep(i),spinlh(3,i)= ", i,idhep(i),spinlh(3,i)
!                   print *, " i,idhep(i),icolorflowlh(:,i)= ", i,idhep(i),icolorflowlh(:,i)
!                end if check_nhepev4
               exit loop_j
             end if check_sumdiff
          end if check_consist
       end do loop_j
    end do loop_i

  end subroutine hepev4_fill

  subroutine hepev4_update(tauspin_pyjets)

    !real(kind=double), dimension(:), intent(in) :: tauspin_pyjets
    real, dimension(:), intent(in) :: tauspin_pyjets
    integer :: i

    spinlh(1:2,nhep_original+1:nhep)=0.
    spinlh(3,nhep_original+1:nhep)=tauspin_pyjets(nhep_original+1:nhep)
    icolorflowlh(:,nhep_original+1:nhep)=0.

!    check_nhepev4: if(nhepev4_call.le.20 .and. msg_level > 2 ) then
!       loop_i: do i=nhep_original+1,nhep
!          if(abs(spinlh(3,i)).gt.epsilon(0.d0)) print *, " i,idhep(i),spinlh(3,i)= ", i,idhep(i),spinlh(3,i)
!       end do loop_i
!    endif check_nhepev4


  end subroutine hepev4_update

end module hepevt_include_common
