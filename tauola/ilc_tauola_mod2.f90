!
!  ILC Tauola interface
!  Revised for Whizard2-Pythia6 based on the ilc_tauola_mod.f90
!  for Whizard1 developped by Timothy Barklow (SLAC)
!
!    Akiya Miyamoto, 21 January 2016
!

module ilc_tauola_mod2
  use hepevt_include_common
  use hepeup_include_common
  implicit none

  private

  public :: ilc_tauola_pytaud
  public :: tauspin_pyjets
  public :: ilc_tauola_get_helicity_mod

  !!! THIS COMMON BLOCK IS USED FOR COMMUNICATION WITH TAUOLA
  common/taupos/np1,np2
  integer                      ::  np1
  integer                      ::  np2

  !!! THIS COMMON BLOCK IS USED FOR COMMUNICATION WITH TAUOLA
  COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
  double precision Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)

  integer  :: nproducts

  integer, parameter                         :: n_pyjets_max=4000
  integer, parameter                         :: nprint_max=20

  double precision                          :: spin_dexay
  double precision, dimension(n_pyjets_max) :: tauspin_pyjets

  double precision, dimension(4)  :: pol

  double precision, parameter            :: a_tau=0.15
  double precision, parameter            :: prob_tau_left_z=(a_tau+1.)/2.

  type, public :: pyjets_spin
    integer           :: index_to_hepeup  ! =-1, if no matching entry in hepeup
    double precision :: helicity   ! copy of SPINUP
    integer           :: pid        ! particle ID
    integer           :: id_orig    ! pid of parent
    integer           :: index_orig ! index of parent
    integer           :: n_daughter ! number of daughter
    integer, dimension(10) :: index_daughter  ! index of daughter particles
  end type pyjets_spin
  type(pyjets_spin), dimension(n_pyjets_max) :: pyjets_spin_data
  integer :: last_event_number = -999

  interface
     function pyr(i_dum)
       implicit none
       double precision :: pyr
       integer, intent(in) :: i_dum
     end function pyr
  end interface

  integer, dimension(200) :: mstp
  double precision, dimension(200) :: parp
  integer, dimension(200) :: msti
  double precision, dimension(200) :: pari
  common/pypars/mstp,parp,msti,pari

  integer :: n
  integer :: npad
  integer, dimension(4000,5) :: k
  double precision, dimension(4000,5) :: p
  double precision, dimension(4000,5) :: v
  common/pyjets/n,npad,k,p,v

contains

  subroutine fill_pyjets_spin_data
    integer :: ip
    integer :: hepeup_index
    integer :: iorig
    integer :: idau1, idau2, n_doc_lines

    ! MSTI(4) ; number of documentation lines
    ! MSTI(5) ; number of events generated
    if ( last_event_number == msti(5) ) then
       return
    end if
    !!! Set helicity information of document lines at the first call of 
    !!! this event
    last_event_number = MSTI(5)

    do ip = 1, n_pyjets_max
       pyjets_spin_data(ip)%index_to_hepeup = -100
       pyjets_spin_data(ip)%helicity = 100
    end do
    ip = 1
    hepeup_index = 0

    do while (k(ip,1) == 21)
       pyjets_spin_data(ip)%pid     = k(ip,2)
       pyjets_spin_data(ip)%id_orig = k(ip,3)
       pyjets_spin_data(ip)%index_orig = ip
       pyjets_spin_data(ip)%n_daughter = 0
       iorig = k(ip,3)
       if (iorig == 0) then
          hepeup_index=hepeup_index+1
          pyjets_spin_data(ip)%index_to_hepeup=hepeup_index
          pyjets_spin_data(ip)%helicity = spinup(hepeup_index)
       else
          pyjets_spin_data(ip)%index_to_hepeup=-1
          pyjets_spin_data(ip)%helicity = 0
          pyjets_spin_data(iorig)%n_daughter = &
               pyjets_spin_data(iorig)%n_daughter+1
          pyjets_spin_data(iorig)%index_daughter(pyjets_spin_data(iorig)%n_daughter)=ip
       end if
       ip=ip+1
    end do
    n_doc_lines = ip - 1

    do ip = 1, n_doc_lines
       ! if h0 decays to tau pairs
       if ( pyjets_spin_data(ip)%pid == 25 .and. &
            pyjets_spin_data(ip)%n_daughter == 2) then
          idau1=pyjets_spin_data(ip)%index_daughter(1)
          idau2=pyjets_spin_data(ip)%index_daughter(2)
          if( abs(pyjets_spin_data(idau1)%pid)==15 .and. &
               (pyjets_spin_data(idau1)%pid+pyjets_spin_data(idau2)%pid)==0) then
             if(pyr(0) < 0.5) then
                pyjets_spin_data(idau1)%helicity=-1
                pyjets_spin_data(idau2)%helicity=1
             else
                pyjets_spin_data(idau1)%helicity=1
                pyjets_spin_data(idau2)%helicity=-1
             end if
          end if

          ! if Z0 decays to tau pairs
       else if ( pyjets_spin_data(ip)%pid == 23 .and. &
            pyjets_spin_data(ip)%n_daughter == 2 ) then
          idau1=pyjets_spin_data(ip)%index_daughter(1)
          idau2=pyjets_spin_data(ip)%index_daughter(2)
          if( abs(pyjets_spin_data(idau1)%pid)==15 .and. &
               (pyjets_spin_data(idau1)%pid+pyjets_spin_data(idau2)%pid)==0 ) then
             if ( ((pyr(0) - prob_tau_left_z ) * pyjets_spin_data(idau1)%pid ) .gt. 0.0 ) then
                pyjets_spin_data(idau1)%helicity=1
                pyjets_spin_data(idau2)%helicity=-1
             else
                pyjets_spin_data(idau1)%helicity=1
                pyjets_spin_data(idau2)%helicity=-1
             end if
          end if

          ! If W+(24) decays to tau+(-15) and neu_tau
       else if ( pyjets_spin_data(ip)%pid == 24 .and. pyjets_spin_data(ip)%n_daughter == 2 ) then
          idau1=pyjets_spin_data(ip)%index_daughter(1)
          idau2=pyjets_spin_data(ip)%index_daughter(2)
          if ( pyjets_spin_data(idau1)%pid == -15 ) then
             pyjets_spin_data(idau1)%helicity=1
          else if ( pyjets_spin_data(idau2)%pid == -15 ) then
             pyjets_spin_data(idau2)%helicity=1
          endif

          ! If W-(-24) decays to tau-(+15) and neu_tau
       else if ( pyjets_spin_data(ip)%pid == -24 .and. pyjets_spin_data(ip)%n_daughter == 2 ) then
          idau1=pyjets_spin_data(ip)%index_daughter(1)
          idau2=pyjets_spin_data(ip)%index_daughter(2)
          if ( pyjets_spin_data(idau1)%pid == 15 ) then
             pyjets_spin_data(idau1)%helicity=-1
          else if ( pyjets_spin_data(idau2)%pid == 15 ) then
             pyjets_spin_data(idau2)%helicity=-1
          endif
       end if
    end do
  end subroutine fill_pyjets_spin_data

  ! ====================================================================
  !  Main interface to tauola.
  !  Called by PYTAUD and calls TAUOLA
  ! ====================================================================
  subroutine ilc_tauola_pytaud(itau,iorig,kforig,ndecay)
    !!! Line number in /JETSET/ where the tau is stored
    integer, intent(in)  :: itau   
    !!! Line number where the mother is stored. =0 if the mother is not stored
    integer, intent(in)  :: iorig  
    !!! Flavour code of the mother. 0 unknown. H0(25), W+-(+-24), 
    !!!   gamma*/Z=23, H+-(+-37)
    integer, intent(in)  :: kforig 
    !!! Number of decay products to be given by user routine.
    integer, intent(out) :: ndecay 
    double precision, dimension(5) :: p_dexay
    integer :: id_dexay
    integer :: ip
    !!! TODO: (bcn 2016-03-11) this should only be 
    !!!       called once per event (not per tau)
    !!! JRR: Tau decays are very sensitive to numerical noise, momenta
    !!!      should be, in principle, strictly on the z axis
    if (abs (p(itau,1)) < 1.d-13)  p(itau,1) = 0
    if (abs (p(itau,2)) < 1.d-13)  p(itau,2) = 0
    call fill_pyjets_spin_data()
    !!! If tau origin is not known ( tau is generated by parton generator ), 
    !!! look parton information, which are stored as status code=21
    if ( iorig == 0 ) then
       ip = itau
       do while (k(ip,3) .ne. 0 .or. ip .le. 0 )
          ip = k(ip,3)
       end do
       id_dexay = k(itau,2)
       p_dexay = p(itau,1:5)
       spin_dexay = pyjets_spin_data(ip)%helicity
       !!! If tau origin is known ( iorig .ne. 0 ), decide tau helicity 
       !!! based on parent particle id (kforig )
       ! kforig=25 | 2 tau's spin must be generated: tau
    else
       id_dexay = k(itau,2)
       p_dexay = p(itau,1:5)
       !!! h0 or z0 case
       if ( kforig == 25 .or. kforig==23 .or. abs(kforig)==24 ) then
          ip = k(itau,3)
          if( k(ip,1)==21 .and. abs(k(ip,2))==15 ) then
             pyjets_spin_data(itau)%helicity = pyjets_spin_data(ip)%helicity
          endif
          spin_dexay = pyjets_spin_data(itau)%helicity
       else ! Unknow decay mother
          print *, 'Warning: In ilc_tau_decay : Unknown decay mother of tau, id=', kforig, ' tau is 50% right or 50% left handed.'
          if ( pyr(0) .lt. 0.5 ) then
             spin_dexay=-1
          else
             spin_dexay=1
          end if
          pyjets_spin_data(itau)%helicity=spin_dexay
       end if
    end if
    call do_dexay (itau, p_dexay, id_dexay)
    ndecay = nproducts
  end subroutine ilc_tauola_pytaud


  subroutine do_dexay (itau, p_dexay, id_dexay)
    integer, intent(in) :: itau, id_dexay
    double precision, dimension(5), intent(in) :: p_dexay
    integer   :: i
    tauspin_pyjets(itau) = spin_dexay
    nhep = 2

    isthep(1) = 1
    idhep(1) = id_dexay
    jmohep(:,1) = 0
    jdahep(:,1) = 0
    phep(:,1) = p_dexay

    isthep(2) = 1
    idhep(2) = - id_dexay
    jmohep(:,2) = 0
    jdahep(:,2) = 0
    phep(1:3,2) = - phep(1:3,1)
    phep(4:5,2) = phep(4:5,1)

    check_tau_sign: if(idhep(1).lt.0) then
       np1 = 1
       np2 = 2
       pol = 0.
       pol(3) = - spin_dexay
       p1 = phep(1:4,1)
       p2 = phep(1:4,2)
       q1 = p1 + p2
       !print *, " antiparticle decay q1= ", q1
       !print *, " antiparticle decay p1= ", p1
       !print *, " antiparticle decay p2= ", p2
       ! ==========================================================
       ! TAUOLA is called here
       ! ==========================================================
       call dexay(1,pol)
    else check_tau_sign
       np2 = 1
       np1 = 2
       pol = 0.
       pol(3) = spin_dexay
       p2 = phep(1:4,1)
       p1 = phep(1:4,2)
       q1 = p1 + p2
       !print *, " particle decay q1= ", q1
       !print *, " particle decay p1= ", p1
       !print *, " particle decay p2= ", p2
       ! ==========================================================
       ! TAUOLA is called here
       ! ==========================================================
       call dexay(2,pol)
       ! ==========================================================
    end if check_tau_sign

    nproducts = 0
    loop_products: do i = 3, nhep
       nproducts = nproducts + 1
       p(n+nproducts,:) = phep(:,i)
       if ( isthep(i) == 1 ) then
          k(n+nproducts,1) = 1
          k(n+nproducts,4) = 0
          k(n+nproducts,5) = 0
       else
          k(n+nproducts,1) = 11
          k(n+nproducts,4) = jdahep(1,i) + n - 2
          k(n+nproducts,5) = jdahep(2,i) + n - 2
       endif
       k(n+nproducts,2) = idhep(i)
       if ( abs(idhep(jmohep(1,i))) .ne. 15 ) then
          k(n+nproducts,3) = jmohep(1,i) + n - 2
       else
          k(n+nproducts,3) = itau
       endif
    end do loop_products
  end subroutine do_dexay

  function ilc_tauola_get_helicity_mod (itau) result (the_helicity)
    integer, intent(in)  :: itau
    integer           :: the_helicity
    integer, dimension(200) :: mstu
    double precision, dimension(200) :: paru
    integer, dimension(200) :: mstj
    double precision, dimension(200) :: parj
    common/pydat1/mstu, paru, mstj, parj
    save/pydat1/
    if ( MSTJ(28) .NE. 2 ) then
       the_helicity=0
    else
      if ( itau .le. 0 .or. itau .gt. n ) then
        the_helicity = 0
      else
        the_helicity = int(pyjets_spin_data(itau)%helicity)
      end if
    end if
  end function ilc_tauola_get_helicity_mod

end module ilc_tauola_mod2

subroutine ilc_tauola_init_call
  use ilc_tauola_mod2
  use hepeup_include_common
  integer, dimension(200) :: mstp
  double precision, dimension(200) :: parp
  integer, dimension(200) :: msti
  double precision, dimension(200) :: pari
  common/pypars/mstp,parp,msti,pari
  INTEGER JAK1, JAK2, JAKP, JAKM, KTOM
  COMMON /JAKI/ JAK1, JAK2, JAKP, JAKM, KTOM
  call tauola(-1,1)
  JAK1 = MSTP(198)
  JAK2 = MSTP(199)
end subroutine ilc_tauola_init_call

subroutine ilc_tauola_get_helicity (itau, the_helicity)
  use ilc_tauola_mod2
  integer, intent(in)  :: itau
  integer, intent(out) :: the_helicity
  the_helicity=ilc_tauola_get_helicity_mod(itau)
end subroutine ilc_tauola_get_helicity

subroutine handle_pytaud(itau,iorig,kforig,ndecay)
  use ilc_tauola_mod2
  implicit none
  integer, intent(in)  :: itau
  integer, intent(in)  :: iorig
  integer, intent(in)  :: kforig
  integer, intent(out) :: ndecay
  call ilc_tauola_pytaud(itau,iorig,kforig,ndecay)
end subroutine handle_pytaud

!*********************************************************************
!...PYTAUD
!...Routine to handle the decay of a polarized tau lepton.
!...Input:
!...ITAU is the position where the decaying tau is stored in /PYJETS/.
!...IORIG is the position where the mother of the tau is stored;
!...     is 0 when the mother is not stored.
!...KFORIG is the flavour of the mother of the tau;
!...     is 0 when the mother is not known.
!...Note that IORIG=0 does not necessarily imply KFORIG=0;
!...     e.g. in B hadron semileptonic decays the W  propagator
!...     is not explicitly stored but the W code is still unambiguous.
!...Output:
!...NDECAY is the number of decay products in the current tau decay.
!...These decay products should be added to the /PYJETS/ common block,
!...in positions N+1 through N+NDECAY. For each product I you must
!...give the flavour codes K(I,2) and the five-momenta P(I,1), P(I,2),
!...P(I,3), P(I,4) and P(I,5). The rest will be stored automatically.
subroutine pytaud (itau,iorig,kforig,ndecay)
  use ilc_tauola_mod2
  implicit none
  integer itau,iorig,kforig
  integer ndecay
  !print *,"###############################################"
  !print *,"###### tauola pytaud was called ###############"
  !print *," itau,iorig,kforig=",itau,iorig,kforig
  !print *,"###############################################"

  call ilc_tauola_pytaud(itau,iorig,kforig,ndecay)

end subroutine pytaud
