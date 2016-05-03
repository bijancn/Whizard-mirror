!
!  WHIZARD Tauola interface
!  Adapted from ilc_tauola_mod.f90
!  for Whizard1 developed by Timothy Barklow (SLAC)
!
!    Akiya Miyamoto
!

module tauola_interface

  use kinds
  use io_units
  use constants
  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use diagnostics
  use hep_common
  use hepev4_aux
  use variables
  use model_data

  implicit none

  private

  public :: ilc_tauola_pytaud
  public :: tauspin_pyjets
  public :: ilc_tauola_get_helicity_mod
  public :: ilc_tauola_init_call
  public :: ilc_tauola_get_helicity
  public :: taudec_settings_t
  public :: pyjets_spin_t

  !!! THIS COMMON BLOCK IS USED FOR COMMUNICATION WITH TAUOLA
  common/taupos/np1,np2
  integer                      ::  np1
  integer                      ::  np2

  !!! THIS COMMON BLOCK IS USED FOR COMMUNICATION WITH TAUOLA
  COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
  double precision Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)

  double precision, external :: wthiggs

  logical, save, public :: trans_spin
  integer :: jtau2, jorig, jforig
  integer :: nproducts

  integer, parameter :: n_pyjets_max = 4000

  integer, save :: max_dump = 0
  integer, save :: nsub_call = 0

  double precision :: spin_dexay
  double precision, dimension(n_pyjets_max) :: tauspin_pyjets

  double precision, dimension(4) :: pol
  logical :: higgs_dec

  !!! Probability of tau- to be left-handed in Z decays
  double precision, parameter :: a_tau = 0.15
  double precision, parameter :: prob_tau_left_z = (a_tau+1.) / 2.

  type :: taudec_settings_t
    logical :: photos
    logical :: transverse
    logical :: dec_rad_cor
    integer :: dec_mode1
    integer :: dec_mode2
    real(default) :: mh
    real(default) :: mix_angle
    real(default) :: mtau
  contains
    procedure :: init => taudec_settings_init
    procedure :: write => taudec_settings_write
  end type taudec_settings_t

  type :: pyjets_spin_t
    integer           :: index_to_hepeup  ! =-1, if no matching entry in hepeup
    double precision  :: helicity   ! copy of SPINUP
    integer           :: pid        ! particle ID
    integer           :: id_orig    ! pid of parent
    integer           :: index_orig ! index of parent
    integer           :: n_daughter ! number of daughter
    integer, dimension(10) :: index_daughter  ! index of daughter particles
  end type pyjets_spin_t

  type(pyjets_spin_t), dimension(n_pyjets_max), save :: pyjets_spin_data
  integer :: last_event_number = -999

  interface
     function pyr(i_dum)
       implicit none
       double precision :: pyr
       integer, intent(in) :: i_dum
     end function pyr
  end interface

contains

  subroutine fill_pyjets_spin_data
    integer :: ip
    integer :: hepeup_index
    integer :: iorig
    integer :: idau1, idau2, n_doc_lines
    integer, dimension(200) :: mstp
    double precision, dimension(200) :: parp
    integer, dimension(200) :: msti
    double precision, dimension(200) :: pari
    common/pypars/mstp,parp,msti,pari
    save/pypars/
    integer :: n
    integer :: npad
    integer, dimension(4000,5) :: k
    double precision, dimension(4000,5) :: p
    double precision, dimension(4000,5) :: v
    common/pyjets/n,npad,k,p,v
    save/pyjets/
    !!! Set helicity information of document lines at the first call of
    !!! this event
    !!! MSTI(4) ; number of documentation lines
    !!! MSTI(5) ; number of events generated
    if (last_event_number == MSTI(5)) then
       return
    end if
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
          hepeup_index = hepeup_index + 1
          pyjets_spin_data(ip)%index_to_hepeup = hepeup_index
          pyjets_spin_data(ip)%helicity = spinup(hepeup_index)
       else
          pyjets_spin_data(ip)%index_to_hepeup = -1
          pyjets_spin_data(ip)%helicity = 0
          pyjets_spin_data(iorig)%n_daughter = &
               pyjets_spin_data(iorig)%n_daughter + 1
          pyjets_spin_data(iorig)%index_daughter(pyjets_spin_data(iorig)%n_daughter)=ip
       end if
       if (debug2_active (D_TAUOLA)) then
          call msg_debug2 (D_TAUOLA, "TAUOLA interface: fill_pyjets_spin_data")
          write (msg_buffer, "(A,I0,A,I0,A,I0,A,I0)") &
               "ip = ", ip, " iorig = ", iorig, " pid  ", k(ip,2), &
               " spin = ", pyjets_spin_data(ip)%helicity
          call msg_message ()
       end if
       if (abs(k(ip,2)) == 15 .and. pyjets_spin_data(ip)%helicity == 9) then
          if (nsub_call .lt. min(5, 2*max_dump)) then
             write (msg_buffer, "(A)") &
                  "Subroutine fill_pyjets_spin_data: tau helicity information"
             call msg_message ()
             write (msg_buffer, "(A)") &
                  "is not set, though polarized tau decay was requested."
             call msg_message ()
             write (msg_buffer, "(A)") &
                  "Most likely, the SINDARIN file does not include polarized"
             call msg_message ()
             write (msg_buffer, "(A)") &
                  "for particles and/or not ?polarized_events=true"
             call msg_message ()
             write (msg_buffer, "(A,I0,A,I0,A,I0,A,I0)") &
                  "Number of calls:", nsub_call, " ip = ", ip, " iorig = ", &
                  iorig, " pid = ", k(ip,2), " spin = ", &
                  pyjets_spin_data(ip)%helicity
          end if
       end if
       ip = ip + 1
    end do
    n_doc_lines = ip - 1

    do ip = 1, n_doc_lines
       if (pyjets_spin_data(ip)%n_daughter == 2) then
          !!! h0/H0/A0 -> tau tau
          if (pyjets_spin_data(ip)%pid == 25 .or. &
               pyjets_spin_data(ip)%pid == 35 .or. &
               pyjets_spin_data(ip)%pid == 36) then
             idau1 = pyjets_spin_data(ip)%index_daughter(1)
             idau2 = pyjets_spin_data(ip)%index_daughter(2)
             if (abs(pyjets_spin_data(idau1)%pid) == 15 .and. &
                  (pyjets_spin_data(idau1)%pid + &
                  pyjets_spin_data(idau2)%pid) == 0) then
                if (pyr(0) .lt. 0.5) then
                   pyjets_spin_data(idau1)%helicity = -1
                   pyjets_spin_data(idau2)%helicity = +1
                else
                   pyjets_spin_data(idau1)%helicity = +1
                   pyjets_spin_data(idau2)%helicity = -1
                end if
             end if

             !!! Z0 -> tau tau
          else if (pyjets_spin_data(ip)%pid == 23) then
             idau1 = pyjets_spin_data(ip)%index_daughter(1)
             idau2 = pyjets_spin_data(ip)%index_daughter(2)
             if (abs(pyjets_spin_data(idau1)%pid) == 15 .and. &
                  (pyjets_spin_data(idau1)%pid + &
                  pyjets_spin_data(idau2)%pid) == 0) then
                if (((pyr(0) - prob_tau_left_z) * &
                     pyjets_spin_data(idau1)%pid) .gt. 0.0) then
                   pyjets_spin_data(idau1)%helicity = +1
                   pyjets_spin_data(idau2)%helicity = -1
                else
                   pyjets_spin_data(idau1)%helicity = -1
                   pyjets_spin_data(idau2)%helicity = +1
                end if
             end if
             
             !!! W+(24)/H+(37) -> tau+(-15) and neu_tau
          else if ( pyjets_spin_data(ip)%pid == 24 .or. &
               pyjets_spin_data(ip)%pid == 37) then
             idau1 = pyjets_spin_data(ip)%index_daughter(1)
             idau2 = pyjets_spin_data(ip)%index_daughter(2)
             if ( pyjets_spin_data(idau1)%pid == -15 ) then
                pyjets_spin_data(idau1)%helicity = 1
             else if ( pyjets_spin_data(idau2)%pid == -15 ) then
                pyjets_spin_data(idau2)%helicity = 1
             end if

             !!! W-(-24)/H-(-37) -> tau-(+15) and neu_tau_bar
          else if (pyjets_spin_data(ip)%pid == -24 .or. &
               pyjets_spin_data(ip)%pid == -37) then
             idau1 = pyjets_spin_data(ip)%index_daughter(1)
             idau2 = pyjets_spin_data(ip)%index_daughter(2)
             if (pyjets_spin_data(idau1)%pid == 15) then
                pyjets_spin_data(idau1)%helicity = -1
             else if (pyjets_spin_data(idau2)%pid == 15) then
                pyjets_spin_data(idau2)%helicity = -1
             end if
          end if
       end if
    end do
  end subroutine fill_pyjets_spin_data

! =====================================================================
!  Main interface to tauola.
!  Called by PYTAUD and calls TAUOLA
! =====================================================================
  subroutine ilc_tauola_pytaud (itau, iorig, kforig, ndecay)
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
    integer :: n
    integer :: npad
    integer, dimension(4000,5) :: k
    double precision, dimension(4000,5) :: p
    double precision, dimension(4000,5) :: v
    common /pyjets/ n, npad, k, p, v
    save /pyjets/
    integer, dimension(200) :: mstp
    double precision, dimension(200) :: parp
    integer, dimension(200) :: msti
    double precision, dimension(200) :: pari
    common /pypars/ mstp, parp, msti, pari
    save /pypars/
    !!! TODO: (bcn 2016-03-11) this should only be
    !!!       called once per event (not per tau)
    integer :: itau1, ktau, idau1, idau2

    higgs_dec = .false.
    if (kforig == 25 .or. kforig==35 .or. kforig== 36) then
      higgs_dec = .true.
    end if

    !!! JRR: Tau decays are very sensitive to numerical noise, momenta
    !!!      should be, in principle, strictly on the z axis
    if (abs (p(itau,1)) < 1.d-13)  p(itau,1) = 0
    if (abs (p(itau,2)) < 1.d-13)  p(itau,2) = 0

    !!! MSTI(4): number of documentation lines
    !!! MSTI(5): number of events generated
    if (last_event_number .ne. MSTI(5)) then
       call fill_pyjets_spin_data
       last_event_number = MSTI(5)
       jtau2 = -1000
       nsub_call = nsub_call + 1
    end if

    if (nsub_call .lt. max_dump) then
       write (msg_buffer, "(A)") "ilc_tauola_pytaud was called."
       call msg_message ()
       write (msg_buffer, "(A,I0,A,I0,A,I0,A,I0)") &
            "ncall = ", nsub_call, "itau = ", itau, " iorig = ", iorig, &
            " kforig = ", kforig
       call msg_message ()
       call pylist(2)
    end if

    jorig = iorig
    jforig = kforig

    !!! If tau origin is not known (tau is generated by parton generator with
    !!! with full matrix elements), look for the parton information, which is
    !!! stored as status code 21
    if (iorig == 0) then
       ip = itau
       do while (k(ip,3) .ne. 0 .or. ip .le. 0)
          ip = k(ip,3)
       end do
       id_dexay = k(itau,2)
       p_dexay = p(itau,1:5)
       pyjets_spin_data(itau)%helicity = pyjets_spin_data(ip)%helicity
       spin_dexay = pyjets_spin_data(ip)%helicity
       !!! Flip the sign of spin in advance, because it is flipped later
       if ( id_dexay .gt. 0 ) then 
          spin_dexay = -spin_dexay
       end if

       !!! If tau origin is known (iorig .ne. 0), decide tau helicity
       !!! based on parent particle id (kforig)
       !!! kforig = 25/35/36: 2 tau's spin must be generated: tau

    else
       id_dexay = k(itau,2)
       p_dexay = p(itau,1:5)
       !!! h0 or Z0 or W case
       if (higgs_dec .or. kforig==23 .or. abs(kforig)==24) then
          ip = k(itau,3)
          if (k(ip,1)==21 .and. abs(k(ip,2))==15) then
             pyjets_spin_data(itau)%helicity = pyjets_spin_data(ip)%helicity
          end if
          spin_dexay = pyjets_spin_data(itau)%helicity

          !!! Fill momentum of second tau if kforig == 25 ( Higgs )
          if (higgs_dec .and. trans_spin) then

             idau1 = pyjets_spin_data(iorig)%index_daughter(1)
             idau2 = pyjets_spin_data(iorig)%index_daughter(2)
             !!! Parent Higgs is not in the documentation line (K(,1) != 21)
             !!! Get pointer to daughter directly from JETSET
             if (idau1 == 0) then 
                idau1 = k(iorig,4)     
                idau2 = k(iorig,5)
             end if
             if (idau1 .ne. itau) then
                write (msg_buffer, "(A,I0,A,I0,A)") &
                     "idau1 = ", idau1, "itau = ", itau, " are not equal."
                call msg_fatal ("ilc_tauola_pytaud: " // &
                     "Something is wrong in parent-daughter relation.")
             end if
             jtau2 = idau2
             !!! Reset tau spin information because it is decided internally
             pyjets_spin_data(itau)%helicity = 0   
             pyjets_spin_data(jtau2)%helicity = 0
          end if
       else
        !!! Unknow decay mother
          call msg_warning ("ilc_tau_decay : Unknown decay modther of " // &
               "tau, id = " // int2char (kforig) // ", tau is 50% right " // &
               "or 50% left handed.")
          if (pyr(0) .lt. 0.5) then
             spin_dexay = -1
          else
             spin_dexay = +1
          end if
          pyjets_spin_data(itau)%helicity = spin_dexay
       end if
    end if
    call do_dexay (itau, p_dexay, id_dexay, kforig)
    ndecay = nproducts
  end subroutine ilc_tauola_pytaud

  subroutine do_dexay (itau, p_dexay, id_dexay, kforig)
    !!! Main routine to call Tauola. Three type of tau decay:
    !!! (A) Higgs to tau+tau-
    !!! (B) single tau+ decay
    !!! (C) single tau- decay, are treated separately
    integer, intent(in) :: itau, id_dexay
    double precision, dimension(5), intent(in) :: p_dexay
    integer, intent(in) :: kforig

    integer :: i, IFPHOT
    logical :: ifpseudo, is_swapped
    double precision, dimension(4) :: pol1, pol2
    integer :: im, idx1, idx2, idsign
    double precision :: rrr(1), wt
    double precision :: hh1(4), hh2(4)
    integer :: ion(3), np
    common /PHOACT/ IFPHOT
    integer :: n
    integer :: npad
    integer, dimension(4000,5) :: k
    double precision, dimension(4000,5) :: p
    double precision, dimension(4000,5) :: v
    common /pyjets/ n, npad, k, p, v
    save /pyjets/

    tauspin_pyjets(itau) = spin_dexay
    !!! change nhep from 2 -> 3 changes tauola_2 test output
    !!! plus changes of 1->2 and 2->3 in the next two blocks
    nhep = 2

    isthep(1)   = 1
    idhep(1)    = id_dexay
    jmohep(:,1) = 0
    jdahep(:,1) = 0
    phep(:,1)   = p_dexay

    isthep(2)   = 1
    idhep(2)    = - id_dexay
    jmohep(:,2) = 0
    jdahep(:,2) = 0
    phep(1:3,2) = - phep(1:3,1)
    phep(4:5,2) = phep(4:5,1)

!!! NOTE (Akiya Miyamoto, 25-March-2016)
!!!  Higgs (h0/H0/A0) to tau+tau- decay is handled here
!!!  in order to implement a transverse spin correlation.
!!!  For this algorithm to work, photon emission from tau
!!!  before decay should be turned off. Since photon emission
!!!  from tau is handled by PYTHIA, photon emission from ALL tau
!!!  should be turned off.  It is done by setting MSTJ(39)=15.
!!!  Instead, PHOTOS is called after tau decay and generate
!!!  photons.

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
    loop_products_nohiggs: do i = 3, nhep
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
       end if
       k(n+nproducts,2) = idhep(i)
       if (abs(idhep(jmohep(1,i))) .ne. 15) then
          k(n+nproducts,3) = jmohep(1,i) + n - 2
       else
          k(n+nproducts,3) = itau
       endif
    end do loop_products_nohiggs
  end subroutine do_dexay

  subroutine taudec_settings_init (taudec_settings, var_list, model)
    class(taudec_settings_t), intent(out) :: taudec_settings
    type(var_list_t), intent(in) :: var_list
    class(model_data_t), intent(in) :: model
    type(field_data_t), pointer :: field
    taudec_settings%photos = &
         var_list%get_lval (var_str ("?ps_tauola_photos"))
    taudec_settings%transverse = &
         var_list%get_lval (var_str ("?ps_tauola_transverse"))
    taudec_settings%dec_rad_cor = &
         var_list%get_lval (var_str ("?ps_tauola_dec_rad_cor"))
    taudec_settings%dec_mode1 = &
         var_list%get_ival (var_str ("ps_tauola_dec_mode1"))
    taudec_settings%dec_mode2 = &
         var_list%get_ival (var_str ("ps_tauola_dec_mode2"))
    taudec_settings%mh = &
         var_list%get_rval (var_str ("ps_tauola_mh"))
    taudec_settings%mix_angle = &
         var_list%get_rval (var_str ("?ps_tauola_mix_angle"))
    select case (char (model%get_name ()))
    case ("QCD", "Test")
       call msg_fatal ("taudec_settings_init: Model has no tau.")
    case default
       field => model%get_field_ptr (15)
       taudec_settings%mtau = field%get_mass ()
    end select
  end subroutine taudec_settings_init

  subroutine taudec_settings_write (taudec_settings, unit)
    class(taudec_settings_t), intent(in) :: taudec_settings
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u<0)  return
    write (u, "(1x,A)")  "Tau decay settings:"
    call write_separator (u)
    write (u, "(3x,A,1x,L1)") &
         "ps_tauola_photos      = ", taudec_settings%photos
    write (u, "(3x,A,1x,L1)") &
         "ps_tauola_transverse  = ", taudec_settings%transverse
    write (u, "(3x,A,1x,L1)") &
         "ps_tauola_dec_rad_cor = ", taudec_settings%dec_rad_cor
    write (u, "(3x,A,1x,I2)") &
         "ps_tauola_dec_mode1   = ", taudec_settings%dec_mode1
    write (u, "(3x,A,1x,I2)") &
         "ps_tauola_dec_mode2   = ", taudec_settings%dec_mode2
    write (u, "(3x,A,1x,ES19.12)") &
         "ps_tauola_mh          = ", taudec_settings%mh
    write (u, "(3x,A,1x,ES19.12)") &
         "ps_tauola_mix_angle   = ", taudec_settings%mix_angle
  end subroutine taudec_settings_write  

  function ilc_tauola_get_helicity_mod (ip) result (the_helicity)
    integer, intent(in)  :: ip
    integer           :: the_helicity
    integer :: n
    integer :: npad
    integer, dimension(4000,5) :: k
    double precision, dimension(4000,5) :: p
    double precision, dimension(4000,5) :: v
    common /pyjets/ n, npad, k, p, v
    save /pyjets/
    integer, dimension(200) :: mstu
    double precision, dimension(200) :: paru
    integer, dimension(200) :: mstj
    double precision, dimension(200) :: parj
    common /pydat1/ mstu, paru, mstj, parj
    save /pydat1/
    if ( MSTJ(28) .NE. 2 ) then
       the_helicity=0
    else
      if ( ip .le. 0 .or. ip .gt. n ) then
        the_helicity = 0
      else
        the_helicity = int(pyjets_spin_data(ip)%helicity)
      end if
    end if
  end function ilc_tauola_get_helicity_mod

  subroutine ilc_tauola_get_helicity (ip, the_helicity)
    integer, intent(in)  :: ip
    integer, intent(out) :: the_helicity
    the_helicity = ilc_tauola_get_helicity_mod(ip)
    if ( abs(the_helicity) .gt. 1 ) then
       write (msg_buffer, "(A,I0,A,I0,A)") &
            "Stored helicity information is wrong: ", the_helicity, &
            "for ip = ", ip, "."
       call msg_warning ()
    end if
  end subroutine ilc_tauola_get_helicity

  subroutine ilc_tauola_init_call (taudec_settings)
    !!! Tauola initialization.
    !!! (default defined in rt_data)
    !!!  JAK1   ! (0) decay mode of first tau
    !!!  JAK2   ! (0) decay mode of second tau
    !!!  ITDKRC ! (1) switch on radiative corrections in decay
    !!!  IFPHOT ! (1) PHOTOS switch
    type(taudec_settings_t), intent(in) :: taudec_settings
    INTEGER JAK1, JAK2, JAKP, JAKM, KTOM
    COMMON /JAKI/ JAK1, JAK2, JAKP, JAKM, KTOM
    
    integer, dimension(200) :: MSTP
    double precision, dimension(200) :: PARP
    integer, dimension(200) :: MSTI
    double precision, dimension(200) :: PARI
    common /PYPARS/ MSTP, PARP, MSTI, PARI
    save /PYPARS/

    integer, dimension(200) :: MSTU
    double precision, dimension(200) :: PARU
    integer, dimension(200) :: MSTJ
    double precision, dimension(200) :: PARJ
    common /PYDAT1/ MSTU, PARU, MSTJ, PARJ
    save /PYDAT1/
    
    integer :: ITDKRC, IFPHOT
    double precision :: psi, betah
    double precision :: csc, ssc
    common /pseudocoup/ csc, ssc
    save /pseudocoup/
    call tauola (-1,1)
    JAK1 = taudec_settings%dec_mode1
    JAK2 = taudec_settings%dec_mode2
    if (taudec_settings%dec_rad_cor) then
       ITDKRC = 1
    else
       ITDKRC = 0
    end if
    if (taudec_settings%photos) then
       IFPHOT = 1
    else
       IFPHOT = 0
    end if

    trans_spin = taudec_settings%transverse

    psi = dble (taudec_settings%mix_angle * degree)
    betah = dble (sqrt (one - four * taudec_settings%mtau**2 / &
         taudec_settings%mh**2))
    csc = cos(psi) * betah
    ssc = sin(psi)

    if (debug2_active (D_TAUOLA)) then
       call msg_debug2 (D_TAUOLA, "TAUOLA initialization")       
       call taudec_settings%write ()
       call msg_debug2 (D_TAUOLA, " check if TAUOLA common block has been set")
       call msg_debug2 (D_TAUOLA, "Tau decay modes set")
       print *, " Tau decay modes: tau+(JAK1) = ", jak1, &
            " tau-(JAK2) = ", JAK2
       call msg_message ("   JAK =  0    : All decay mode")
       call msg_message ("   JAK =  1    : electron mode")
       call msg_message ("   JAK =  2    : muon mode")
       call msg_message ("   JAK =  3    : pion mode")
       call msg_message ("   JAK =  4    : rho mode")
       call msg_message ("   JAK =  5    : a1 mode")
       call msg_message ("   JAK =  6    : K mode")
       call msg_message ("   JAK =  7    : K* mode")
       call msg_message ("   JAK =  8-13 : n pion modes")
       call msg_message ("   JAK = 14-19 : K K pi and K pi pi modes")
       call msg_message ("   JAK = 20-21 : eta pi pi; gamma pi pi modes")
       call msg_debug2 (D_TAUOLA, "Radiative corrections in decay ON(1),Off(0)")
       print *, " ITDKRC = ", ITDKRC
       call msg_debug2 (D_TAUOLA, "PHOTOS switch: ON(1), OFF(0)")
       print *, " IFPHOT = ", IFPHOT
    end if
  end subroutine ilc_tauola_init_call

end module tauola_interface


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
subroutine pytaud (itau, iorig, kforig, ndecay)
  use tauola_interface
  implicit none
  integer itau,iorig,kforig
  integer ndecay
  !print *,"###############################################"
  !print *,"###### tauola pytaud was called ###############"
  !print *," itau,iorig,kforig=",itau,iorig,kforig
  !print *,"###############################################"

  call ilc_tauola_pytaud (itau, iorig, kforig, ndecay)

end subroutine pytaud
