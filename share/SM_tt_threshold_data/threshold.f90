! File generated automatically by O'Mega
!
!   /scratch/bcho/trunk/_install/develop/bin/omega_SM_tt_threshold.opt -o ttbar_i1.f90 -target:whizard -target:parameter_module parameters_SM_tt_threshold -target:module opr_ttbar_i1 -target:md5sum 3C3BACDC1B40C40DB783E084F0B62DFC -fusion:progress -scatter "e- e+ -> W+ W- b bbar" -cascade "3+5~t && 4+6~tbar"
!
! with all scattering amplitudes for the process(es)
!
!   flavor combinations:
!
!       1: e- e+ -> W+ W- b bbar
!
!   color flows:
!
!       1: (  0,  0) (  0,  0) -> (  0,  0) (  0,  0) (  1,  0) (  0, -1)
!
!     NB: i.g. not all color flows contribute to all flavor
!     combinations.  Consult the array FLV_COL_IS_ALLOWED
!     below for the allowed combinations.
!
!   Color Factors:
!
!     (  1,  1): + N
!
!   vanishing or redundant flavor combinations:
!
!
!   diagram selection (MIGHT BREAK GAUGE INVARIANCE!!!):
!
!     (3+5 ~ t) && (4+6 ~ tbar)  grouping {{3,5},{4,6}}
!
! in minimal electroweak standard model in unitarity gauge
!
module @ID@_threshold
  use kinds
  use omega95
  use parameters_SM_tt_threshold
  implicit none
  private
  public :: init, md5sum, calculate_amplitudes

  ! DON'T EVEN THINK of removing the following!
  ! If the compiler complains about undeclared
  ! or undefined variables, you are compiling
  ! against an incompatible omega95 module!
  integer, dimension(7), parameter, private :: require = &
    (/ omega_spinors_2010_01_A, omega_spinor_cpls_2010_01_A, &
       omega_vectors_2010_01_A, omega_polarizations_2010_01_A, &
       omega_couplings_2010_01_A, omega_color_2010_01_A, &
       omega_utils_2010_01_A /)

  integer, parameter, public :: n_prt = 6
  integer, parameter :: n_prt_OS = 4
  integer, parameter, public :: n_in = 2
  integer, parameter, public :: n_out = 4
  integer, parameter, public :: n_cflow = 1
  integer, parameter, public :: n_cindex = 2
  integer, parameter, public :: n_flv = 1
  integer, parameter, public :: n_hel = 144
  integer, parameter :: n_hel_OS = 16

  ! NB: you MUST NOT change the value of N_ here!!!
  !     It is defined here for convenience only and must be
  !     compatible with hardcoded values in the amplitude!
  real(kind=default), parameter, public :: N_ = 3
  logical, parameter :: F = .false.
  logical, parameter :: T = .true.

  integer, dimension(n_prt_OS,n_hel_OS), save, protected :: table_spin_states_OS
  data table_spin_states_OS(:,   1) / -1, -1, -1, -1 /
  data table_spin_states_OS(:,   2) / -1, -1, -1,  1 /
  data table_spin_states_OS(:,   3) / -1, -1,  1, -1 /
  data table_spin_states_OS(:,   4) / -1, -1,  1,  1 /
  data table_spin_states_OS(:,   5) / -1,  1, -1, -1 /
  data table_spin_states_OS(:,   6) / -1,  1, -1,  1 /
  data table_spin_states_OS(:,   7) / -1,  1,  1, -1 /
  data table_spin_states_OS(:,   8) / -1,  1,  1,  1 /
  data table_spin_states_OS(:,   9) /  1, -1, -1, -1 /
  data table_spin_states_OS(:,  10) /  1, -1, -1,  1 /
  data table_spin_states_OS(:,  11) /  1, -1,  1, -1 /
  data table_spin_states_OS(:,  12) /  1, -1,  1,  1 /
  data table_spin_states_OS(:,  13) /  1,  1, -1, -1 /
  data table_spin_states_OS(:,  14) /  1,  1, -1,  1 /
  data table_spin_states_OS(:,  15) /  1,  1,  1, -1 /
  data table_spin_states_OS(:,  16) /  1,  1,  1,  1 /

  integer, dimension(n_prt,n_hel), save, protected :: table_spin_states
  data table_spin_states(:,   1) / -1, -1, -1, -1, -1, -1 /
  data table_spin_states(:,   2) / -1, -1, -1, -1, -1,  1 /
  data table_spin_states(:,   3) / -1, -1, -1, -1,  1, -1 /
  data table_spin_states(:,   4) / -1, -1, -1, -1,  1,  1 /
  data table_spin_states(:,   5) / -1, -1, -1,  0, -1, -1 /
  data table_spin_states(:,   6) / -1, -1, -1,  0, -1,  1 /
  data table_spin_states(:,   7) / -1, -1, -1,  0,  1, -1 /
  data table_spin_states(:,   8) / -1, -1, -1,  0,  1,  1 /
  data table_spin_states(:,   9) / -1, -1, -1,  1, -1, -1 /
  data table_spin_states(:,  10) / -1, -1, -1,  1, -1,  1 /
  data table_spin_states(:,  11) / -1, -1, -1,  1,  1, -1 /
  data table_spin_states(:,  12) / -1, -1, -1,  1,  1,  1 /
  data table_spin_states(:,  13) / -1, -1,  0, -1, -1, -1 /
  data table_spin_states(:,  14) / -1, -1,  0, -1, -1,  1 /
  data table_spin_states(:,  15) / -1, -1,  0, -1,  1, -1 /
  data table_spin_states(:,  16) / -1, -1,  0, -1,  1,  1 /
  data table_spin_states(:,  17) / -1, -1,  0,  0, -1, -1 /
  data table_spin_states(:,  18) / -1, -1,  0,  0, -1,  1 /
  data table_spin_states(:,  19) / -1, -1,  0,  0,  1, -1 /
  data table_spin_states(:,  20) / -1, -1,  0,  0,  1,  1 /
  data table_spin_states(:,  21) / -1, -1,  0,  1, -1, -1 /
  data table_spin_states(:,  22) / -1, -1,  0,  1, -1,  1 /
  data table_spin_states(:,  23) / -1, -1,  0,  1,  1, -1 /
  data table_spin_states(:,  24) / -1, -1,  0,  1,  1,  1 /
  data table_spin_states(:,  25) / -1, -1,  1, -1, -1, -1 /
  data table_spin_states(:,  26) / -1, -1,  1, -1, -1,  1 /
  data table_spin_states(:,  27) / -1, -1,  1, -1,  1, -1 /
  data table_spin_states(:,  28) / -1, -1,  1, -1,  1,  1 /
  data table_spin_states(:,  29) / -1, -1,  1,  0, -1, -1 /
  data table_spin_states(:,  30) / -1, -1,  1,  0, -1,  1 /
  data table_spin_states(:,  31) / -1, -1,  1,  0,  1, -1 /
  data table_spin_states(:,  32) / -1, -1,  1,  0,  1,  1 /
  data table_spin_states(:,  33) / -1, -1,  1,  1, -1, -1 /
  data table_spin_states(:,  34) / -1, -1,  1,  1, -1,  1 /
  data table_spin_states(:,  35) / -1, -1,  1,  1,  1, -1 /
  data table_spin_states(:,  36) / -1, -1,  1,  1,  1,  1 /
  data table_spin_states(:,  37) / -1,  1, -1, -1, -1, -1 /
  data table_spin_states(:,  38) / -1,  1, -1, -1, -1,  1 /
  data table_spin_states(:,  39) / -1,  1, -1, -1,  1, -1 /
  data table_spin_states(:,  40) / -1,  1, -1, -1,  1,  1 /
  data table_spin_states(:,  41) / -1,  1, -1,  0, -1, -1 /
  data table_spin_states(:,  42) / -1,  1, -1,  0, -1,  1 /
  data table_spin_states(:,  43) / -1,  1, -1,  0,  1, -1 /
  data table_spin_states(:,  44) / -1,  1, -1,  0,  1,  1 /
  data table_spin_states(:,  45) / -1,  1, -1,  1, -1, -1 /
  data table_spin_states(:,  46) / -1,  1, -1,  1, -1,  1 /
  data table_spin_states(:,  47) / -1,  1, -1,  1,  1, -1 /
  data table_spin_states(:,  48) / -1,  1, -1,  1,  1,  1 /
  data table_spin_states(:,  49) / -1,  1,  0, -1, -1, -1 /
  data table_spin_states(:,  50) / -1,  1,  0, -1, -1,  1 /
  data table_spin_states(:,  51) / -1,  1,  0, -1,  1, -1 /
  data table_spin_states(:,  52) / -1,  1,  0, -1,  1,  1 /
  data table_spin_states(:,  53) / -1,  1,  0,  0, -1, -1 /
  data table_spin_states(:,  54) / -1,  1,  0,  0, -1,  1 /
  data table_spin_states(:,  55) / -1,  1,  0,  0,  1, -1 /
  data table_spin_states(:,  56) / -1,  1,  0,  0,  1,  1 /
  data table_spin_states(:,  57) / -1,  1,  0,  1, -1, -1 /
  data table_spin_states(:,  58) / -1,  1,  0,  1, -1,  1 /
  data table_spin_states(:,  59) / -1,  1,  0,  1,  1, -1 /
  data table_spin_states(:,  60) / -1,  1,  0,  1,  1,  1 /
  data table_spin_states(:,  61) / -1,  1,  1, -1, -1, -1 /
  data table_spin_states(:,  62) / -1,  1,  1, -1, -1,  1 /
  data table_spin_states(:,  63) / -1,  1,  1, -1,  1, -1 /
  data table_spin_states(:,  64) / -1,  1,  1, -1,  1,  1 /
  data table_spin_states(:,  65) / -1,  1,  1,  0, -1, -1 /
  data table_spin_states(:,  66) / -1,  1,  1,  0, -1,  1 /
  data table_spin_states(:,  67) / -1,  1,  1,  0,  1, -1 /
  data table_spin_states(:,  68) / -1,  1,  1,  0,  1,  1 /
  data table_spin_states(:,  69) / -1,  1,  1,  1, -1, -1 /
  data table_spin_states(:,  70) / -1,  1,  1,  1, -1,  1 /
  data table_spin_states(:,  71) / -1,  1,  1,  1,  1, -1 /
  data table_spin_states(:,  72) / -1,  1,  1,  1,  1,  1 /
  data table_spin_states(:,  73) /  1, -1, -1, -1, -1, -1 /
  data table_spin_states(:,  74) /  1, -1, -1, -1, -1,  1 /
  data table_spin_states(:,  75) /  1, -1, -1, -1,  1, -1 /
  data table_spin_states(:,  76) /  1, -1, -1, -1,  1,  1 /
  data table_spin_states(:,  77) /  1, -1, -1,  0, -1, -1 /
  data table_spin_states(:,  78) /  1, -1, -1,  0, -1,  1 /
  data table_spin_states(:,  79) /  1, -1, -1,  0,  1, -1 /
  data table_spin_states(:,  80) /  1, -1, -1,  0,  1,  1 /
  data table_spin_states(:,  81) /  1, -1, -1,  1, -1, -1 /
  data table_spin_states(:,  82) /  1, -1, -1,  1, -1,  1 /
  data table_spin_states(:,  83) /  1, -1, -1,  1,  1, -1 /
  data table_spin_states(:,  84) /  1, -1, -1,  1,  1,  1 /
  data table_spin_states(:,  85) /  1, -1,  0, -1, -1, -1 /
  data table_spin_states(:,  86) /  1, -1,  0, -1, -1,  1 /
  data table_spin_states(:,  87) /  1, -1,  0, -1,  1, -1 /
  data table_spin_states(:,  88) /  1, -1,  0, -1,  1,  1 /
  data table_spin_states(:,  89) /  1, -1,  0,  0, -1, -1 /
  data table_spin_states(:,  90) /  1, -1,  0,  0, -1,  1 /
  data table_spin_states(:,  91) /  1, -1,  0,  0,  1, -1 /
  data table_spin_states(:,  92) /  1, -1,  0,  0,  1,  1 /
  data table_spin_states(:,  93) /  1, -1,  0,  1, -1, -1 /
  data table_spin_states(:,  94) /  1, -1,  0,  1, -1,  1 /
  data table_spin_states(:,  95) /  1, -1,  0,  1,  1, -1 /
  data table_spin_states(:,  96) /  1, -1,  0,  1,  1,  1 /
  data table_spin_states(:,  97) /  1, -1,  1, -1, -1, -1 /
  data table_spin_states(:,  98) /  1, -1,  1, -1, -1,  1 /
  data table_spin_states(:,  99) /  1, -1,  1, -1,  1, -1 /
  data table_spin_states(:, 100) /  1, -1,  1, -1,  1,  1 /
  data table_spin_states(:, 101) /  1, -1,  1,  0, -1, -1 /
  data table_spin_states(:, 102) /  1, -1,  1,  0, -1,  1 /
  data table_spin_states(:, 103) /  1, -1,  1,  0,  1, -1 /
  data table_spin_states(:, 104) /  1, -1,  1,  0,  1,  1 /
  data table_spin_states(:, 105) /  1, -1,  1,  1, -1, -1 /
  data table_spin_states(:, 106) /  1, -1,  1,  1, -1,  1 /
  data table_spin_states(:, 107) /  1, -1,  1,  1,  1, -1 /
  data table_spin_states(:, 108) /  1, -1,  1,  1,  1,  1 /
  data table_spin_states(:, 109) /  1,  1, -1, -1, -1, -1 /
  data table_spin_states(:, 110) /  1,  1, -1, -1, -1,  1 /
  data table_spin_states(:, 111) /  1,  1, -1, -1,  1, -1 /
  data table_spin_states(:, 112) /  1,  1, -1, -1,  1,  1 /
  data table_spin_states(:, 113) /  1,  1, -1,  0, -1, -1 /
  data table_spin_states(:, 114) /  1,  1, -1,  0, -1,  1 /
  data table_spin_states(:, 115) /  1,  1, -1,  0,  1, -1 /
  data table_spin_states(:, 116) /  1,  1, -1,  0,  1,  1 /
  data table_spin_states(:, 117) /  1,  1, -1,  1, -1, -1 /
  data table_spin_states(:, 118) /  1,  1, -1,  1, -1,  1 /
  data table_spin_states(:, 119) /  1,  1, -1,  1,  1, -1 /
  data table_spin_states(:, 120) /  1,  1, -1,  1,  1,  1 /
  data table_spin_states(:, 121) /  1,  1,  0, -1, -1, -1 /
  data table_spin_states(:, 122) /  1,  1,  0, -1, -1,  1 /
  data table_spin_states(:, 123) /  1,  1,  0, -1,  1, -1 /
  data table_spin_states(:, 124) /  1,  1,  0, -1,  1,  1 /
  data table_spin_states(:, 125) /  1,  1,  0,  0, -1, -1 /
  data table_spin_states(:, 126) /  1,  1,  0,  0, -1,  1 /
  data table_spin_states(:, 127) /  1,  1,  0,  0,  1, -1 /
  data table_spin_states(:, 128) /  1,  1,  0,  0,  1,  1 /
  data table_spin_states(:, 129) /  1,  1,  0,  1, -1, -1 /
  data table_spin_states(:, 130) /  1,  1,  0,  1, -1,  1 /
  data table_spin_states(:, 131) /  1,  1,  0,  1,  1, -1 /
  data table_spin_states(:, 132) /  1,  1,  0,  1,  1,  1 /
  data table_spin_states(:, 133) /  1,  1,  1, -1, -1, -1 /
  data table_spin_states(:, 134) /  1,  1,  1, -1, -1,  1 /
  data table_spin_states(:, 135) /  1,  1,  1, -1,  1, -1 /
  data table_spin_states(:, 136) /  1,  1,  1, -1,  1,  1 /
  data table_spin_states(:, 137) /  1,  1,  1,  0, -1, -1 /
  data table_spin_states(:, 138) /  1,  1,  1,  0, -1,  1 /
  data table_spin_states(:, 139) /  1,  1,  1,  0,  1, -1 /
  data table_spin_states(:, 140) /  1,  1,  1,  0,  1,  1 /
  data table_spin_states(:, 141) /  1,  1,  1,  1, -1, -1 /
  data table_spin_states(:, 142) /  1,  1,  1,  1, -1,  1 /
  data table_spin_states(:, 143) /  1,  1,  1,  1,  1, -1 /
  data table_spin_states(:, 144) /  1,  1,  1,  1,  1,  1 /

  integer, dimension(n_prt,n_flv), save, protected :: table_flavor_states
  data table_flavor_states(:,   1) /  11, -11,  24, -24,   5,  -5 / ! e- e+ W+ W- b bbar

  complex(default), dimension(n_hel), save, public :: amp_ff, &
       amp_A_v_tree, amp_A_v_blob, amp_Z_av_tree, amp_Z_av_blob

  type(momentum) :: p1, p2, p3, p4, p5, p6
  type(momentum) :: p12, p35, p46
  type(spinor) :: owf_t_4, owf_b_6, owf_e_1
  type(conjspinor) :: owf_t_3, owf_b_5, owf_e_2
  type(vector) :: owf_Wm_3, owf_Wp_4
  type(spinor) :: owf_wb_46
  type(conjspinor) :: owf_wb_35
  type(vector) :: owf_A_12, owf_Z_12

contains

  pure function md5sum ()
    character(len=32) :: md5sum
    ! DON'T EVEN THINK of modifying the following line!
    md5sum = "3C3BACDC1B40C40DB783E084F0B62DFC"
  end function md5sum

  subroutine init (par)
    real(kind=default), dimension(*), intent(in) :: par
    call import_from_whizard (par)
  end subroutine init

  subroutine calculate_amplitudes (k)
    real(kind=default), dimension(0:3,*), intent(in) :: k
    complex(default) :: blob_Z_vec, blob_Z_ax
    integer, dimension(n_prt) :: s
    integer, dimension(n_prt_OS) :: s_OS
    integer :: hi
    p1 = - k(:,1) ! incoming
    p2 = - k(:,2) ! incoming
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p5 =   k(:,5) ! outgoing
    p6 =   k(:,6) ! outgoing
    p12 = p1 + p2
    p35 = p3 + p5
    p46 = p4 + p6
    amp_Z_av_tree = zero
    amp_Z_av_tree = zero
    amp_A_v_tree = zero
    amp_A_v_blob = zero
    if (onshell_tops (p3, p4)) then
       do hi = 1, n_hel_OS
          s_OS = table_spin_states_OS(:,hi)
          owf_e_1 = u (mass(11), - p1, s_OS(1))
          owf_e_2 = vbar (mass(11), - p2, s_OS(2))
          owf_t_3 = ubar (ttv_mtpole(p12*p12), p3, s_OS(3))
          owf_t_4 = v (ttv_mtpole(p12*p12), p4, s_OS(4))

          owf_A_12 = pr_feynman (p12, v_ff (qlep, owf_e_2, owf_e_1))
          owf_Z_12 = pr_unitarity (p12, mass(23), wd_tl (p12, width(23)), &
               + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))

          blob_Z_vec = gncup(1) * ttv_formfactor (p3, p4, 1)
          blob_Z_ax = gncup(2) * ttv_formfactor (p3, p4, 2)

          amp_Z_av_tree(hi) = owf_Z_12 * va_ff (gncup(1), gncup(2), owf_t_3, owf_t_4)
          amp_Z_av_blob(hi) = owf_Z_12 * va_ff (blob_Z_vec, blob_Z_ax, owf_t_3, owf_t_4)

          amp_A_v_tree(hi) = owf_A_12 * v_ff (qup, owf_t_3, owf_t_4)
          amp_A_v_blob(hi) = amp_A_v_tree(hi) * ttv_formfactor (p3, p4, 1)
       end do
    else
       do hi = 1, n_hel
          s = table_spin_states(:,hi)
          owf_e_1 = u (mass(11), - p1, s(1))
          owf_e_2 = vbar (mass(11), - p2, s(2))
          owf_Wm_3 = conjg (eps (mass(24), p3, s(3)))
          owf_Wp_4 = conjg (eps (mass(24), p4, s(4)))
          owf_b_5 = ubar (mass(5), p5, s(5))
          owf_b_6 = v (mass(5), p6, s(6))

          owf_A_12 = pr_feynman(p12, v_ff (qlep, owf_e_2, owf_e_1))
          owf_Z_12 = pr_unitarity(p12, mass(23), wd_tl (p12, width(23)), &
               + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))

          owf_wb_35 = pr_psibar (p35, ttv_mtpole (p12*p12), wd_tl (p35, width(6)), &
               + f_fvl (gccq33, owf_b_5, owf_Wm_3))
          owf_wb_46 = pr_psi (p46, ttv_mtpole(p12*p12), wd_tl (p46, width(6)), &
               + f_vlf (gccq33, owf_Wp_4, owf_b_6))

          blob_Z_vec = gncup(1) * ttv_formfactor (p35, p46, 1)
          blob_Z_ax = gncup(2) * ttv_formfactor (p35, p46, 2)

          amp_Z_av_tree(hi) = owf_Z_12 * va_ff (gncup(1), gncup(2), owf_wb_35, owf_wb_46)
          amp_Z_av_blob(hi) = owf_Z_12 * va_ff (blob_Z_vec, blob_Z_ax, owf_wb_35, owf_wb_46)

          amp_A_v_tree(hi) = owf_A_12 * v_ff (qup, owf_wb_35, owf_wb_46)
          amp_A_v_blob(hi) = amp_A_v_tree(hi) * ttv_formfactor (p35, p46, 1)
       end do
    end if
    amp_ff = amp_A_v_tree + amp_A_v_blob + amp_Z_av_tree + amp_Z_av_blob
  end subroutine calculate_amplitudes

end module @ID@_threshold

! alphas will be set in ttv_formfactors
! warning: this only works with SM_tt_threshold. As this model will
!        also be used for the full diagrams, we should disable the
!        va_ilc_tta/z there
subroutine threshold_init (par) bind(C)
  use iso_c_binding
  use kinds
  use @ID@_threshold
  implicit none
  real(c_default_float), dimension(*), intent(in) :: par
  call init (par)
end subroutine threshold_init

!subroutine @ID@_threshold_get_amplitude_squared (p) bind(C)
subroutine threshold_get_amp_squared (amp2, p) bind(C)
  use iso_c_binding
  use kinds
  use opr_@ID@, sm_new_event => new_event
  use opr_@ID@, sm_get_amplitude => get_amplitude
  use @ID@_threshold
  use parameters_sm_tt_threshold
  implicit none
  real(c_default_float), intent(out) :: amp2
  real(c_default_float), dimension(0:3,*), intent(in) :: p
  !complex(default) :: amp_sm
  !integer :: hi
  !call sm_new_event (p)
  call calculate_amplitudes (p)
  amp2 = 0.0_default
  select case (FF)
  case (3,4)
     amp2 = sum (amp_A_v_tree * conjg (amp_A_v_tree) + &
                 amp_A_v_tree * conjg (amp_A_v_blob) + &
                 amp_A_v_tree * conjg (amp_Z_av_tree) + &
                 amp_A_v_tree * conjg (amp_Z_av_blob) + &
                 amp_A_v_blob * conjg (amp_A_v_tree) + &
                 amp_A_v_blob * conjg (amp_Z_av_tree) + &
                 amp_Z_av_tree * conjg (amp_A_v_tree) + &
                 amp_Z_av_tree * conjg (amp_A_v_blob) + &
                 amp_Z_av_tree * conjg (amp_Z_av_tree) + &
                 amp_Z_av_tree * conjg (amp_Z_av_blob) + &
                 amp_Z_av_blob * conjg (amp_A_v_tree) + &
                 amp_Z_av_blob * conjg (amp_Z_av_tree))
  case default
     !do hi = 1, n_hel
        !amp_sm = sm_get_amplitude (1, hi, 1)
        !amp2 = amp2 + real(amp_sm * conjg(amp_ff(hi)))
        !amp2 = amp2 + real(amp_ff(hi) * conjg(amp_ff(hi)))
     !end do
     amp2 = sum ((amp_A_v_tree + amp_A_v_blob + amp_Z_av_tree + amp_Z_av_blob) * &
           conjg (amp_A_v_tree + amp_A_v_blob + amp_Z_av_tree + amp_Z_av_blob))
  end select
  amp2 = amp2 * N_ / 4.0_default
end subroutine threshold_get_amp_squared
!end subroutine @ID@_threshold_get_amplitude_squared
