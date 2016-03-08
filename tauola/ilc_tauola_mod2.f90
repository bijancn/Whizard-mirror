!
!  ILC Tauola interface
!  Revised for Whizard2-Pythia6 based on the ilc_tauola_mod.f90
!  for Whizard1 developped by Timothy Barklow (SLAC)
!
!    Akiya Miyamoto, 21 January 2016
!

module ilc_tauola_mod2
  !use kinds, only: single,double
  use hepevt_include_common
  use hepeup_include_common
  use taupos_include_common
  use momdec_include_common
  use pyr_inter
  use pycomp_inter
  use pyjets_common
!  use diagnostics, only: msg_level
  implicit none

  private

  public :: ilc_tauola_pytaud
  public :: tauspin_pyjets
  public :: ilc_tauola_print_helicity_info_mod
  public :: ilc_tauola_get_helicity_mod

  integer :: n_ini_call=0
! integer, parameter :: n_ini_call0=4360
  integer, parameter :: n_ini_call0=30971

  integer  :: jtau
  integer  :: jorig
  integer  :: jforig
  integer  :: nproducts

  integer, parameter                         :: n_pyjets_max=4000
  integer                                    :: id_dexay
  integer, parameter                         :: nprint_max=20

  !real(kind=double), dimension(5)            :: p_dexay
  real, dimension(5)            :: p_dexay
  !real(kind=double)                          :: spin_dexay
  real                          :: spin_dexay
  !real(kind=double), dimension(n_pyjets_max) :: tauspin_pyjets
  real, dimension(n_pyjets_max) :: tauspin_pyjets

  !real(kind=single), dimension(4)  :: pol
  real, dimension(4)  :: pol

  !real(kind=double), parameter            :: a_tau=0.15
  real, parameter            :: a_tau=0.15
  !real(kind=double), parameter            :: prob_tau_left_z=(a_tau+1.)/2.
  real, parameter            :: prob_tau_left_z=(a_tau+1.)/2.

  type, public :: pyjets_spin
    integer           :: index_to_hepeup  ! =-1, if no matching entry in hepeup
    !real(kind=double) :: helicity   ! copy of SPINUP
    real :: helicity   ! copy of SPINUP
    integer           :: pid        ! particle ID
    integer           :: id_orig    ! pid of parent
    integer           :: index_orig ! index of parent
    integer           :: n_daughter ! number of daughter
    integer, dimension(10) :: index_daughter  ! index of daughter particles
  end type pyjets_spin
  type(pyjets_spin), dimension(n_pyjets_max) :: pyjets_spin_data
  integer :: last_event_number = -999

  integer, dimension(200) :: mstp
  !real(kind=double), dimension(200) :: parp
  real, dimension(200) :: parp
  integer, dimension(200) :: msti
  !real(kind=double), dimension(200) :: pari
  real, dimension(200) :: pari
  common/pypars/mstp,parp,msti,pari

contains

  subroutine fill_pyjets_spin_data
    integer :: ip
    integer :: hepeup_index
    logical :: is_document_line
    integer :: iorig
    integer :: idau1, idau2, n_doc_lines

! MSTI(4) ; number of documentation lines
! MSTI(5) ; number of events generated

    if ( last_event_number .eq. msti(5) ) then
       return
    end if

!
! Set helicity information of document lines at the first call of this event
!
    last_event_number=MSTI(5)

    do ip=1, n_pyjets_max
      pyjets_spin_data(ip)%index_to_hepeup=-100
      pyjets_spin_data(ip)%helicity=100
    end do

    ip = 1
    hepeup_index=0
    is_document_line=.true.

    do while ( k(ip,1) .eq. 21 )
      pyjets_spin_data(ip)%pid     = k(ip,2)
      pyjets_spin_data(ip)%id_orig = k(ip,3)
      pyjets_spin_data(ip)%index_orig = ip
      pyjets_spin_data(ip)%n_daughter = 0
      iorig=k(ip,3)
      if( iorig .eq. 0 ) then
        hepeup_index=hepeup_index+1
        pyjets_spin_data(ip)%index_to_hepeup=hepeup_index
        pyjets_spin_data(ip)%helicity = spinup(hepeup_index)
      else
        pyjets_spin_data(ip)%index_to_hepeup=-1
        pyjets_spin_data(ip)%helicity = 0
        pyjets_spin_data(iorig)%n_daughter = pyjets_spin_data(iorig)%n_daughter+1
        pyjets_spin_data(iorig)%index_daughter(pyjets_spin_data(iorig)%n_daughter)=ip
      end if
      ip=ip+1
    end do
    n_doc_lines=ip-1

    do ip = 1, n_doc_lines
! if h0 decays to tau pairs
      if ( pyjets_spin_data(ip)%pid .eq. 25 .and. pyjets_spin_data(ip)%n_daughter .eq. 2 ) then
        idau1=pyjets_spin_data(ip)%index_daughter(1)
        idau2=pyjets_spin_data(ip)%index_daughter(2)
        if( abs(pyjets_spin_data(idau1)%pid).eq.15 .and. (pyjets_spin_data(idau1)%pid+pyjets_spin_data(idau2)%pid).eq.0 ) then
          if( pyr(0) .lt. 0.5 ) then
            pyjets_spin_data(idau1)%helicity=-1
            pyjets_spin_data(idau2)%helicity=1
          else
            pyjets_spin_data(idau1)%helicity=1
            pyjets_spin_data(idau2)%helicity=-1
          end if
        end if

! if Z0 decays to tau pairs
      else if ( pyjets_spin_data(ip)%pid .eq. 23 .and. pyjets_spin_data(ip)%n_daughter .eq. 2 ) then
        idau1=pyjets_spin_data(ip)%index_daughter(1)
        idau2=pyjets_spin_data(ip)%index_daughter(2)
        if( abs(pyjets_spin_data(idau1)%pid).eq.15 .and. (pyjets_spin_data(idau1)%pid+pyjets_spin_data(idau2)%pid).eq.0 ) then
          if ( ((pyr(0) - prob_tau_left_z ) * pyjets_spin_data(idau1)%pid ) .gt. 0.0 ) then
            pyjets_spin_data(idau1)%helicity=1
            pyjets_spin_data(idau2)%helicity=-1
          else
            pyjets_spin_data(idau1)%helicity=1
            pyjets_spin_data(idau2)%helicity=-1
          end if
        end if

! If W+(24) decays to tau+(-15) and neu_tau
      else if ( pyjets_spin_data(ip)%pid .eq. 24 .and. pyjets_spin_data(ip)%n_daughter .eq. 2 ) then
        idau1=pyjets_spin_data(ip)%index_daughter(1)
        idau2=pyjets_spin_data(ip)%index_daughter(2)
        if ( pyjets_spin_data(idau1)%pid .eq. -15 ) then
             pyjets_spin_data(idau1)%helicity=1
        else if ( pyjets_spin_data(idau2)%pid .eq. -15 ) then
             pyjets_spin_data(idau2)%helicity=1
        endif

! If W-(-24) decays to tau-(+15) and neu_tau
      else if ( pyjets_spin_data(ip)%pid .eq. -24 .and. pyjets_spin_data(ip)%n_daughter .eq. 2 ) then
        idau1=pyjets_spin_data(ip)%index_daughter(1)
        idau2=pyjets_spin_data(ip)%index_daughter(2)
        if ( pyjets_spin_data(idau1)%pid .eq. 15 ) then
             pyjets_spin_data(idau1)%helicity=-1
        else if ( pyjets_spin_data(idau2)%pid .eq. 15 ) then
             pyjets_spin_data(idau2)%helicity=-1
        endif
      end if

    end do
!
  end subroutine fill_pyjets_spin_data


! ========================================================================================================
!  Main interface to tauola.
!  Called by PYTAUD through handle_pytaud and calls TAUOLA
! ========================================================================================================
!
  subroutine ilc_tauola_pytaud(itau,iorig,kforig,ndecay)
    integer, intent(in)  :: itau  ! Line number in /JETSET/ where the tau is stored
    integer, intent(in)  :: iorig ! Line numner where the mother is stored. =0 if the mother is not stored
    integer, intent(in)  :: kforig ! Flavour code of the mother. 0 unknown. H0(25), W+-(+-24), gamma*/Z=23, H+-(+-37)
    integer, intent(out) :: ndecay ! Number of decay products to be given by user routine.

    integer  :: iakf
    integer  :: i
    integer :: ip
    integer :: inext
    integer :: itau1, ktau

    jtau=itau
    jorig=iorig
    jforig=kforig

    call fill_pyjets_spin_data

! If tau origin is not known ( tau is generated by parton generator ), look parton information,
! which are stored as status code=21
    if ( iorig .eq. 0 ) then
      ip=itau
      do while (k(ip,3) .ne. 0 .or. ip .le. 0 )
        inext = k(ip,3)
        ip = inext
      end do
      id_dexay=k(jtau,2)
      p_dexay=p(jtau,1:5)
      spin_dexay = pyjets_spin_data(ip)%helicity

!
! If tau origin is known ( iorig .ne. 0 ), decide tau helicity based on parent particle id (kforig )
!   kforig=25 | 2 tau's spin must be generated: tau
!
    else
      id_dexay=k(jtau,2)
      p_dexay=p(jtau,1:5)

!### h0 or z0 case
      if ( kforig .eq. 25 .or. kforig.eq.23 .or. abs(kforig).eq.24 ) then
        ip = k(itau,3)
        if( k(ip,1).eq.21 .and. abs(k(ip,2)).eq.15 ) then
          pyjets_spin_data(itau)%helicity = pyjets_spin_data(ip)%helicity
        endif

        spin_dexay=pyjets_spin_data(itau)%helicity

!### Unknow decay mother
      else
        print *,'In ilc_tau_decay : Unknown decay modther of tau, id=',kforig,' tau is 50% right or 50% left handed.'
        if ( pyr(0) .lt. 0.5 ) then
          spin_dexay=-1
        else
          spin_dexay=1
        end if
        pyjets_spin_data(itau)%helicity=spin_dexay
      end if

    end if

    call do_dexay

    ndecay=nproducts
!!!    call pyhepc(2) ! Convert pyjet common to /pyjets/
    return

  end subroutine ilc_tauola_pytaud

  subroutine do_dexay

    integer   :: i

    tauspin_pyjets(jtau)=spin_dexay

    nhep=2

    isthep(1)=1
    idhep(1)=id_dexay
    jmohep(:,1)=0
    jdahep(:,1)=0
    phep(:,1)=p_dexay

    isthep(2)=1
    idhep(2)=-id_dexay
    jmohep(:,2)=0
    jdahep(:,2)=0
    phep(1:3,2)=-phep(1:3,1)
    phep(4:5,2)=phep(4:5,1)

    check_tau_sign: if(idhep(1).lt.0) then
       np1=1
       np2=2
       pol=0.
       pol(3)=-spin_dexay
       p1=phep(1:4,1)
       p2=phep(1:4,2)
       q1=p1+p2
!       print *, " antiparticle decay q1= ", q1
!       print *, " antiparticle decay p1= ", p1
!       print *, " antiparticle decay p2= ", p2
! ==========================================================
! TAUOLA is called here
! ==========================================================
       call dexay(1,pol)
    else check_tau_sign
       np2=1
       np1=2
       pol=0.
       pol(3)=spin_dexay
       p2=phep(1:4,1)
       p1=phep(1:4,2)
       q1=p1+p2
!       print *, " particle decay q1= ", q1
!       print *, " particle decay p1= ", p1
!       print *, " particle decay p2= ", p2

! ==========================================================
! TAUOLA is called here
! ==========================================================
       call dexay(2,pol)
! ==========================================================
    end if check_tau_sign

    nproducts=0



    loop_products: do i=3,nhep
       nproducts=nproducts+1
       p(n+nproducts,:)=phep(:,i)
       if ( isthep(i) .eq. 1 ) then
          k(n+nproducts,1)=1
          k(n+nproducts,4)=0
          k(n+nproducts,5)=0
       else
          k(n+nproducts,1)=11
          k(n+nproducts,4)=jdahep(1,i)+n-2
          k(n+nproducts,5)=jdahep(2,i)+n-2
        endif
       k(n+nproducts,2)=idhep(i)
       if ( abs(idhep(jmohep(1,i))) .ne. 15 ) then
          k(n+nproducts,3)=jmohep(1,i)+n-2
       else
          k(n+nproducts,3)=jtau
       endif
    end do loop_products

    return

  end subroutine do_dexay

  subroutine ilc_tauola_print_helicity_info_mod
    integer :: ip

  end subroutine ilc_tauola_print_helicity_info_mod

  function ilc_tauola_get_helicity_mod (ip) result (the_helicity)
    integer, intent(in)  :: ip
    integer           :: the_helicity

    integer, dimension(200) :: mstu
    !real(kind=double), dimension(200) :: paru
    real, dimension(200) :: paru
    integer, dimension(200) :: mstj
    !real(kind=double), dimension(200) :: parj
    real, dimension(200) :: parj
    common/pydat1/mstu, paru, mstj, parj
    save/pydat1/

    if ( MSTJ(28) .NE. 2 ) then
       the_helicity=0
    else
      if ( ip .le. 0 .or. ip .gt. n ) then
        the_helicity = 0
      else
!      if ( pyjets_spin_data(ip)%helicity .lt. -1.0 .or. pyjets_spin_data(ip)%helicity .gt. 1.0 ) then
!        the_helicity = 0
!      else
        the_helicity = int(pyjets_spin_data(ip)%helicity)
!      end if
      end if
    endif

  end function ilc_tauola_get_helicity_mod

end module ilc_tauola_mod2


subroutine ilc_tauola_init_call
  use ilc_tauola_mod2
  use hepeup_include_common

    !C...PYTHIA commonblock: only used to prxvidnteger, dimension(200) :: mstp
    integer, dimension(200) :: mstp
    !real(kind=double), dimension(200) :: parp
    real, dimension(200) :: parp
    integer, dimension(200) :: msti
    !real(kind=double), dimension(200) :: pari
    real, dimension(200) :: pari
    common /pypars/ mstp, parp, msti, pari
    save /pypars/

  print *,'########################################################'
  print *,' ilc_tauola_init_call has been called. Must be called from PYINIT '
  print *,'########################################################'

  call tauola(-1,1)
  jak1 = mstp(198)
  jak2 = mstp(199)
  print *,"INIT TAUOLA user fragment init jak1,jak2= ",jak1,jak2

  return
end subroutine ilc_tauola_init_call

subroutine ilc_tauola_print_helicity_info
  use ilc_tauola_mod2
  call ilc_tauola_print_helicity_info_mod
  return
end subroutine ilc_tauola_print_helicity_info

subroutine ilc_tauola_get_helicity (ip, the_helicity)
  use ilc_tauola_mod2
  integer, intent(in)  :: ip
  integer, intent(out) :: the_helicity
  the_helicity=ilc_tauola_get_helicity_mod(ip)
end subroutine ilc_tauola_get_helicity







