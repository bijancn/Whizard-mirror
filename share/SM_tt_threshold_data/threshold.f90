module @ID@_threshold

  implicit none
  use omega95
  use omega_color, OCF => omega_color_factor
  use parameters_SM_tt_threshold

  private
  public :: number_particles_in, number_particles_out, number_color_indices, &
    reset_helicity_selection, new_event, is_allowed, get_amplitude, &
    color_sum, openmp_supported, number_spin_states, spin_states, &
    number_flavor_states, flavor_states, number_color_flows, color_flows, &
    number_color_factors, color_factors, init, final, update_alpha_s, md5sum

  ! DON'T EVEN THINK of removing the following!
  ! If the compiler complains about undeclared
  ! or undefined variables, you are compiling
  ! against an incompatible omega95 module!
  integer, dimension(7), parameter, private :: require = &
    (/ omega_spinors_2010_01_A, omega_spinor_cpls_2010_01_A, &
       omega_vectors_2010_01_A, omega_polarizations_2010_01_A, &
       omega_couplings_2010_01_A, omega_color_2010_01_A, &
       omega_utils_2010_01_A /)

  integer, parameter :: n_prt = 6
  integer, parameter :: n_in = 2
  integer, parameter :: n_out = 4
  integer, parameter :: n_cflow = 1
  integer, parameter :: n_cindex = 2
  integer, parameter :: n_flv = 1
  integer, parameter :: n_hel = 144

  ! NB: you MUST NOT change the value of N_ here!!!
  !     It is defined here for convenience only and must be
  !     compatible with hardcoded values in the amplitude!
  real(kind=default), parameter :: N_ = 3
  logical, parameter :: F = .false.
  logical, parameter :: T = .true.

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

  integer, dimension(n_cindex,n_prt,n_cflow), save, protected :: table_color_flows
  data table_color_flows(:,:,   1) / 0,0,  0,0,  0,0,  0,0,  1,0,  0,-1 /

  logical, dimension(n_prt,n_cflow), save, protected :: table_ghost_flags
  data table_ghost_flags(:,   1) / F,  F,  F,  F,  F,  F /

  integer, parameter :: n_cfactors = 1
  type(OCF), dimension(n_cfactors), save, protected :: table_color_factors
  real(kind=default), parameter, private :: color_factor_000001 = +N_
  data table_color_factors(     1) / OCF(1,1,color_factor_000001) /

  logical, dimension(n_flv, n_cflow), save, protected ::  flv_col_is_allowed
  data flv_col_is_allowed(:,   1) / T /

  complex(kind=default), dimension(n_flv, n_cflow, n_hel), save :: amp

  logical, dimension(n_hel), save :: hel_is_allowed = T
  real(kind=default), dimension(n_hel), save :: hel_max_abs = 0
  real(kind=default), save :: hel_sum_abs = 0, hel_threshold = 1E10
  integer, save :: hel_count = 0, hel_cutoff = 100
  integer :: i
  integer, save, dimension(n_hel) :: hel_map = (/(i, i = 1, n_hel)/)
  integer, save :: hel_finite = n_hel

    type(momentum) :: p1, p2, p3, p4, p5, p6
    type(momentum) :: p12, p35, p46
    type(spinor) :: owf_d3_1__6_0, owf_l1_1_0
    type(conjspinor) :: owf_d3b__1_5_0, owf_l1b_2_0
    type(vector) :: owf_wm_3_0, owf_wp_4_0
    type(spinor) :: owf_u3_1__46_0
    type(conjspinor) :: owf_u3b__1_35_0
    type(vector) :: owf_a_12_0, owf_z_12_0
    complex(kind=default) :: oks_l1l1bwpwmd3_1_d3b__1

contains

  subroutine init (par)
    real(kind=default), dimension(*), intent(in) :: par
    call import_from_whizard (par)
  end subroutine init

  subroutine update_alpha_s (alpha_s)
    real(kind=default), intent(in) :: alpha_s
    call model_update_alpha_s (alpha_s)
  end subroutine update_alpha_s

  subroutine calculate_amplitudes (amp, k)
    complex(kind=default), dimension(:,:,:), intent(out) :: amp
    real(kind=default), dimension(0:3,*), intent(in) :: k
    integer, dimension(n_prt) :: s
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
    amp = 0
    do hi = 1, hel_finite
      s = table_spin_states(:,hi)
      owf_l1_1_0 = u (mass(11), - p1, s(1))
      owf_l1b_2_0 = vbar (mass(11), - p2, s(2))
      owf_wm_3_0 = conjg (eps (mass(24), p3, s(3)))
      owf_wp_4_0 = conjg (eps (mass(24), p4, s(4)))
      owf_d3b__1_5_0 = ubar (mass(5), p5, s(5))
      owf_d3_1__6_0 = v (mass(5), p6, s(6))
      owf_a_12_0 = pr_feynman(p12, + v_ff(qlep,owf_l1b_2_0,owf_l1_1_0))
      owf_z_12_0 = pr_unitarity(p12,mass(23),wd_tl(p12,width(23)), &
         + va_ff(gnclep(1),gnclep(2),owf_l1b_2_0,owf_l1_1_0))
      owf_u3b__1_35_0 = &
         pr_psibar(p35,ttv_mtpole(p12*p12),wd_tl(p35,width(6)), &
         + f_fvl(gccq33,owf_d3b__1_5_0,owf_wm_3_0))
      owf_u3_1__46_0 = pr_psi(p46,ttv_mtpole(p12*p12),wd_tl(p46,width(6)), &
         + f_vlf(gccq33,owf_wp_4_0,owf_d3_1__6_0))
      oks_l1l1bwpwmd3_1_d3b__1 = 0
      oks_l1l1bwpwmd3_1_d3b__1 = oks_l1l1bwpwmd3_1_d3b__1 + owf_z_12_0*( &
         + va_ff(gncup(1),gncup(2),owf_u3b__1_35_0,owf_u3_1__46_0) &
         + va_ff(va_ilc_ttz(p35,p46,1),0*va_ilc_ttz(p35,p46,2),owf_u3b__1_35_0,owf_u3_1__46_0))
      oks_l1l1bwpwmd3_1_d3b__1 = oks_l1l1bwpwmd3_1_d3b__1 + owf_a_12_0*( &
         + v_ff(qup,owf_u3b__1_35_0,owf_u3_1__46_0) &
         + va_ff(va_ilc_tta(p35,p46,1),va_ilc_tta(p35,p46,2),owf_u3b__1_35_0,owf_u3_1__46_0))
      oks_l1l1bwpwmd3_1_d3b__1 = &
         - oks_l1l1bwpwmd3_1_d3b__1 ! 4 vertices, 3 propagators
      amp(1,1,hi) = oks_l1l1bwpwmd3_1_d3b__1
    end do
  end subroutine calculate_amplitudes

end module @ID@_threshold
