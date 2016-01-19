module @ID@_threshold
  use kinds
  use omega95
  use parameters_SM_tt_threshold
  use ttv_formfactors
  implicit none
  private
  public :: init, md5sum, calculate_blob, compute_born, &
       set_production_momenta, init_workspace, compute_owfs

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

  complex(default), dimension(n_hel,0:3), save, public :: amp_blob
  complex(default), dimension(n_hel), save, public :: amp_tree
  integer, public :: nhel_max, ffi_end

  type(momentum) :: p1, p2, p3, p4, p5, p6
  type(momentum) :: p12, p35, p46
  type(spinor) :: owf_t_4, owf_b_6, owf_e_1
  type(conjspinor) :: owf_t_3, owf_b_5, owf_e_2
  type(vector) :: owf_Wm_3, owf_Wp_4
  type(spinor) :: owf_wb_46
  type(conjspinor) :: owf_wb_35
  type(vector) :: owf_A_12, owf_Z_12
  integer, dimension(0:3) :: ff_modes

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

  subroutine compute_owfs (hi)
    integer, intent(in) :: hi
    integer, dimension(n_prt_OS) :: s_OS
    integer, dimension(n_prt) :: s
    if (onshell_tops (p3, p4)) then
       s_OS = table_spin_states_OS(:,hi)
       owf_e_1 = u (mass(11), - p1, s_OS(1))
       owf_e_2 = vbar (mass(11), - p2, s_OS(2))
       owf_t_3 = ubar (ttv_mtpole(p12*p12), p3, s_OS(3))
       owf_t_4 = v (ttv_mtpole(p12*p12), p4, s_OS(4))
       owf_A_12 = pr_feynman (p12, v_ff (qlep, owf_e_2, owf_e_1))
       owf_Z_12 = pr_unitarity (p12, mass(23), wd_tl (p12, width(23)), &
            + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))
    else
       s = table_spin_states(:,hi)
       owf_e_1 = u (mass(11), - p1, s(1))
       owf_e_2 = vbar (mass(11), - p2, s(2))
       owf_Wm_3 = conjg (eps (mass(24), p3, s(3)))
       owf_Wp_4 = conjg (eps (mass(24), p4, s(4)))
       owf_b_5 = ubar (mass(5), p5, s(5))
       owf_b_6 = v (mass(5), p6, s(6))
       owf_A_12 = pr_feynman (p12, v_ff (qlep, owf_e_2, owf_e_1))
       owf_Z_12 = pr_unitarity (p12, mass(23), wd_tl (p12, width(23)), &
            + va_ff (gnclep(1), gnclep(2), owf_e_2, owf_e_1))
    end if
  end subroutine compute_owfs

  function calculate_blob (hi, ffi, h_t, h_tbar) result (this_amp)
    complex(default) :: this_amp
    integer, intent(in) :: hi, ffi
    integer, intent(in), optional :: h_t, h_tbar
    complex(default) :: blob_Z_vec, blob_Z_ax, ttv_vec, ttv_ax
    real(default) :: m, w
    if (onshell_tops (p3, p4)) then
       blob_Z_vec = gncup(1) * ttv_formfactor (p3, p4, 1)
       blob_Z_ax = gncup(2) * ttv_formfactor (p3, p4, 2)
       this_amp = owf_Z_12 * &
            va_ff (blob_Z_vec, blob_Z_ax, owf_t_3, owf_t_4)
       this_amp = this_amp + owf_A_12 * &
            v_ff (qup, owf_t_3, owf_t_4) * ttv_formfactor (p3, p4, 1)
    else
       ttv_vec = ttv_formfactor (p35, p46, 1, ff_modes(ffi))
       ttv_ax = ttv_formfactor (p35, p46, 2, ff_modes(ffi))
       blob_Z_vec = gncup(1) * ttv_vec
       blob_Z_ax = gncup(2) * ttv_ax
       if (OFFSHELL_STRATEGY < 0) then
          owf_t_3 = ubar (sqrt (p35 * p35), p35, h_t)
          owf_t_4 = v (sqrt (p46 * p46), p46, h_tbar)
          this_amp = owf_Z_12 * &
               va_ff (blob_Z_vec, blob_Z_ax, owf_t_3, owf_t_4)
          this_amp = this_amp + owf_A_12 * &
               v_ff (qup, owf_t_3, owf_t_4) * ttv_vec
          m = ttv_mtpole (p12*p12)
          w = ttv_wtpole (p12*p12, ff_modes(ffi))
          this_amp = this_amp / cmplx (p35*p35 - m**2, m*w, kind=default)
          this_amp = this_amp / cmplx (p46*p46 - m**2, m*w, kind=default)
          this_amp = - this_amp
       else
          owf_wb_35 = pr_psibar (p35, ttv_mtpole (p12*p12), &
               wd_tl (p35, ttv_wtpole (p12*p12, ff_modes(ffi))), &
               + f_fvl (gccq33, owf_b_5, owf_Wm_3))
          owf_wb_46 = pr_psi (p46, ttv_mtpole(p12*p12), &
               wd_tl (p46, ttv_wtpole (p12*p12, ff_modes(ffi))), &
               + f_vlf (gccq33, owf_Wp_4, owf_b_6))
          this_amp = owf_Z_12 * &
               va_ff (blob_Z_vec, blob_Z_ax, owf_wb_35, owf_wb_46)
          this_amp = this_amp + owf_A_12 * &
               v_ff (qup, owf_wb_35, owf_wb_46) * ttv_vec
       end if
    end if
  end function calculate_blob

  function decay_me(h_t, h_tbar) result (me)
    complex(default) :: me
    integer, intent(in) :: h_t, h_tbar
    owf_t_3 = vbar (sqrt(p46*p46), p46, h_tbar)
    owf_t_4 = u (sqrt(p35*p35), p35, h_t)
    me = (f_fvl (gccq33, owf_b_5, owf_Wm_3) * owf_t_4) * &
         (owf_t_3 * f_vlf (gccq33, owf_Wp_4, owf_b_6))
    ! TODO: (bcn 2016-01-18) should this be multiplied with N_ ?
    if (OFFSHELL_STRATEGY == -2) then
       me = (f_fvl (gccq33, owf_b_5, owf_Wm_3) * owf_t_4) * &
            (owf_t_3 * f_vlf (gccq33, owf_Wp_4, owf_b_6))
       !!! Not sure this makes sense
       me = me * top_width_nlo (sqrt(p46*p46)) / top_width_lo (sqrt(p46*p46))
       me = me * top_width_nlo (sqrt(p35*p35)) / top_width_lo (sqrt(p35*p35))
    end if
  end function decay_me

  subroutine compute_born (k)
    real(kind=default), dimension(0:3,*), intent(in) :: k
    integer :: hi, ffi, h_t, h_tbar
    call set_production_momenta (k)
    call init_workspace ()
    do hi = 1, nhel_max
       call compute_owfs (hi)
       if (OFFSHELL_STRATEGY < 0) then
          do ffi = 0, ffi_end
             do h_t = -1, 1, 2
                do h_tbar = -1, 1, 2
                   amp_blob(hi,ffi) = amp_blob(hi,ffi) + &
                        calculate_blob (hi, ffi, h_t, h_tbar) * decay_me (h_t, h_tbar)
                end do
             end do
          end do
       else
          do ffi = 0, ffi_end
             amp_blob(hi,ffi) = calculate_blob (hi, ffi)
          end do
       end if
    end do
    amp_blob = - amp_blob ! 4 vertices, 3 propagators
  end subroutine compute_born

  subroutine init_workspace ()
    amp_blob = zero
    ff_modes(0:3) = [FF, EXPANDED_HARD_P0CONSTANT, EXPANDED_SOFT_HARD_P0CONSTANT, &
                     EXPANDED_SOFT_SWITCHOFF_P0CONSTANT]
    if (FF == MATCHED) then
       ffi_end = 3
    else
       ffi_end = 0
    end if
    if (onshell_tops (p3, p4)) then
       nhel_max = n_hel_OS
    else
       nhel_max = n_hel
    end if

  end subroutine init_workspace

  subroutine set_production_momenta (k)
    real(kind=default), dimension(0:3,*), intent(in) :: k
    p1 = - k(:,1) ! incoming
    p2 = - k(:,2) ! incoming
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p5 =   k(:,5) ! outgoing
    p6 =   k(:,6) ! outgoing
    p12 = p1 + p2
    p35 = p3 + p5
    p46 = p4 + p6
  end subroutine set_production_momenta

end module @ID@_threshold

module @ID@_real_decay
  use kinds
  use omega95
  use omega_color, OCF => omega_color_factor
  use parameters_SM
  implicit none
  private
  public :: reset_helicity_selection, calculate_amplitude, is_allowed, get_amplitude, &
    number_spin_states, spin_states, init, update_alpha_s, md5sum

  integer, dimension(7), parameter, private :: require = &
    (/ omega_spinors_2010_01_A, omega_spinor_cpls_2010_01_A, &
       omega_vectors_2010_01_A, omega_polarizations_2010_01_A, &
       omega_couplings_2010_01_A, omega_color_2010_01_A, &
       omega_utils_2010_01_A /)

  integer, parameter :: n_prt = 4
  integer, parameter :: n_in = 1
  integer, parameter :: n_out = 3
  integer, parameter :: n_cflow = 2
  integer, parameter :: n_cindex = 2
  integer, parameter :: n_flv = 1
  integer, parameter :: n_hel = 24

  real(kind=default), parameter :: N_ = 3
  logical, parameter :: F = .false.
  logical, parameter :: T = .true.

  integer, dimension(n_prt,n_hel), save, protected :: table_spin_states
  data table_spin_states(:,   1) / -1, -1, -1, -1 /
  data table_spin_states(:,   2) / -1, -1, -1,  1 /
  data table_spin_states(:,   3) / -1, -1,  1, -1 /
  data table_spin_states(:,   4) / -1, -1,  1,  1 /
  data table_spin_states(:,   5) / -1,  0, -1, -1 /
  data table_spin_states(:,   6) / -1,  0, -1,  1 /
  data table_spin_states(:,   7) / -1,  0,  1, -1 /
  data table_spin_states(:,   8) / -1,  0,  1,  1 /
  data table_spin_states(:,   9) / -1,  1, -1, -1 /
  data table_spin_states(:,  10) / -1,  1, -1,  1 /
  data table_spin_states(:,  11) / -1,  1,  1, -1 /
  data table_spin_states(:,  12) / -1,  1,  1,  1 /
  data table_spin_states(:,  13) /  1, -1, -1, -1 /
  data table_spin_states(:,  14) /  1, -1, -1,  1 /
  data table_spin_states(:,  15) /  1, -1,  1, -1 /
  data table_spin_states(:,  16) /  1, -1,  1,  1 /
  data table_spin_states(:,  17) /  1,  0, -1, -1 /
  data table_spin_states(:,  18) /  1,  0, -1,  1 /
  data table_spin_states(:,  19) /  1,  0,  1, -1 /
  data table_spin_states(:,  20) /  1,  0,  1,  1 /
  data table_spin_states(:,  21) /  1,  1, -1, -1 /
  data table_spin_states(:,  22) /  1,  1, -1,  1 /
  data table_spin_states(:,  23) /  1,  1,  1, -1 /
  data table_spin_states(:,  24) /  1,  1,  1,  1 /

  integer, dimension(n_prt,n_flv), save, protected :: table_flavor_states
  data table_flavor_states(:,   1) /   6,  24,   5,  21 / ! t W+ b gl

  integer, dimension(n_cindex,n_prt,n_cflow), save, protected :: table_color_flows
  data table_color_flows(:,:,   1) / 1,0,  0,0,  1,0,  0,0 /
  data table_color_flows(:,:,   2) / 2,0,  0,0,  1,0,  2,-1 /

  logical, dimension(n_prt,n_cflow), save, protected :: table_ghost_flags
  data table_ghost_flags(:,   1) / F,  F,  F,  T /
  data table_ghost_flags(:,   2) / F,  F,  F,  F /

  integer, parameter :: n_cfactors = 2
  type(OCF), dimension(n_cfactors), save, protected :: table_color_factors
  real(kind=default), parameter, private :: color_factor_000001 = -one
  data table_color_factors(     1) / OCF(1,1,color_factor_000001) /
  real(kind=default), parameter, private :: color_factor_000002 = +N_**2
  data table_color_factors(     2) / OCF(2,2,color_factor_000002) /

  logical, dimension(n_flv, n_cflow), save, protected ::  flv_col_is_allowed
  data flv_col_is_allowed(:,   1) / T /
  data flv_col_is_allowed(:,   2) / T /

  complex(kind=default), dimension(n_flv, n_cflow, n_hel), save :: amp

  logical, dimension(n_hel), save :: hel_is_allowed = T
  real(kind=default), dimension(n_hel), save :: hel_max_abs = 0
  real(kind=default), save :: hel_sum_abs = 0, hel_threshold = 1E10
  integer, save :: hel_count = 0, hel_cutoff = 100
  integer :: i
  integer, save, dimension(n_hel) :: hel_map = (/(i, i = 1, n_hel)/)
  integer, save :: hel_finite = n_hel

    type(momentum) :: p1, p2, p3, p4
    type(momentum) :: p12, p14
    type(spinor) :: owf_u3_2__1_0, owf_u3_1__1_0
    type(conjspinor) :: owf_d3b__1_3_0
    type(vector) :: owf_gl___4_0, owf_gl_1_2_4_0, owf_wm_2_0
    type(spinor) :: owf_d3_2__12_0, owf_d3_1__12_0, owf_u3_1__14_0_X1, &
      owf_u3_1__14_0_X2
    complex(kind=default) :: oks_u3_2_wpd3_1_gl_2_1, oks_u3_1_wpd3_1_gl__

contains

  pure function md5sum ()
    character(len=32) :: md5sum
    ! DON'T EVEN THINK of modifying the following line!
    md5sum = "CC564AA882B2456F7BE6449F8C4BC713"
  end function md5sum

  subroutine init (par)
    real(kind=default), dimension(*), intent(in) :: par
    call import_from_whizard (par)
  end subroutine init

  subroutine update_alpha_s (alpha_s)
    real(kind=default), intent(in) :: alpha_s
    call model_update_alpha_s (alpha_s)
  end subroutine update_alpha_s

  pure function number_spin_states () result (n)
    integer :: n
    n = size (table_spin_states, dim=2)
  end function number_spin_states

  pure subroutine spin_states (a)
    integer, dimension(:,:), intent(out) :: a
    a = table_spin_states
  end subroutine spin_states

  subroutine reset_helicity_selection (threshold, cutoff)
    real(kind=default), intent(in) :: threshold
    integer, intent(in) :: cutoff
    integer :: i
    hel_is_allowed = T
    hel_max_abs = 0
    hel_sum_abs = 0
    hel_count = 0
    hel_threshold = threshold
    hel_cutoff = cutoff
    hel_map = (/(i, i = 1, n_hel)/)
    hel_finite = n_hel
  end subroutine reset_helicity_selection

  pure function is_allowed (flv, hel, col) result (yorn)
    logical :: yorn
    integer, intent(in) :: flv, hel, col
    yorn = hel_is_allowed(hel) .and. flv_col_is_allowed(flv,col)
  end function is_allowed

  pure function get_amplitude (flv, hel, col) result (amp_result)
    complex(kind=default) :: amp_result
    integer, intent(in) :: flv, hel, col
    amp_result = amp(flv, col, hel)
  end function get_amplitude

  function calculate_amplitude (k, s) result (amp)
    complex(kind=default) :: amp
    real(kind=default), dimension(0:3,*), intent(in) :: k
    integer, dimension(n_prt), intent(in) :: s
    integer :: h, hi
    p1 = - k(:,1) ! incoming
    p2 =   k(:,2) ! outgoing
    p3 =   k(:,3) ! outgoing
    p4 =   k(:,4) ! outgoing
    p12 = p1 + p2
    p14 = p1 + p4
    amp = 0
    if (hel_finite == 0) return
    owf_u3_1__1_0 = u (mass(6), - p1, s(1))
    owf_wm_2_0 = conjg (eps (mass(24), p2, s(2)))
    owf_d3b__1_3_0 = ubar (mass(5), p3, s(3))
    owf_gl___4_0 = conjg (eps (mass(21), p4, s(4)))
    owf_d3_1__12_0 = pr_psi(p12,mass(5),wd_tl(p12,width(5)), &
       + f_vlf(gcc,owf_wm_2_0,owf_u3_1__1_0))
    owf_u3_1__14_0_X1 = pr_psi(p14,mass(6),wd_tl(p14,width(6)), &
       + f_vf((-gs),owf_gl___4_0,owf_u3_1__1_0))
    oks_u3_1_wpd3_1_gl__ = ( &
       + f_fv((-gs),owf_d3b__1_3_0,owf_gl___4_0))*owf_d3_1__12_0
    oks_u3_1_wpd3_1_gl__ = oks_u3_1_wpd3_1_gl__ + ( &
       + f_fvl(gcc,owf_d3b__1_3_0,owf_wm_2_0))*owf_u3_1__14_0_X1
    amp = - oks_u3_1_wpd3_1_gl__ ! 2 vertices, 1 propagators
  end subroutine compute_brakets_0001

end module @ID@_real_decay

! alphas will be set in ttv_formfactors
! warning: this only works with SM_tt_threshold. As this model will
!        also be used for the full diagrams, we should disable the
!        va_ilc_tta/z there
subroutine @ID@_threshold_init (par) bind(C)
  use iso_c_binding
  use kinds
  use @ID@_threshold
  implicit none
  real(c_default_float), dimension(*), intent(in) :: par
  call init (par)
end subroutine @ID@_threshold_init

subroutine @ID@_compute_real (amp2, k)
  use kinds
  use constants
  use @ID@_real_decay, real_decay_calculate_amplitude => calculate_amplitude
  use @ID@_real_decay, real_decay_spin_states => spin_states
  use @ID@_threshold
  !use ttv_formfactors
  use parameters_SM_tt_threshold
  implicit none
  real(c_default_float), intent(out) :: amp2
  real(kind=default), dimension(0:3,*), intent(in) :: k
  real(kind=default), dimension(0:3,6) :: k_production
  real(kind=default), dimension(0:3,4) :: k_decay_real
  real(kind=default), dimension(0:3,3) :: k_decay_born
  complex(default) :: production_me
  integer, dimension(2), parameter :: ass_quark = [5, 6]
  integer, dimension(2), parameter :: ass_boson = [3, 4]
  integer :: i, hi, leg, other_leg, ffi, h_t, h_tbar
  call init_decay_and_production_momenta ()
  do leg = 1, 2
     other_leg = 3 - leg
     call set_decay_and_production_momenta ()
     do hi = 1, nhel_max
        call compute_owfs (hi)
        if (OFFSHELL_STRATEGY < 0) then
           do ffi = 0, ffi_end
              do h_t = -1, 1, 2
                 do h_tbar = -1, 1, 2
                    production_me = calculate_blob (hi, ffi, h_t, h_tbar) * &
                         (N_**2 - 1) / N_
                    do h_gl = -1, 1, 2
                       amp_blob(hi,ffi) = amp_blob(hi,ffi) + &
                            real_decay_calculate_amplitude (k_decay_real, [h_t,h_W,h_b,h_gl])
                    end do
                 end do
              end do
           end do
        else
           print *, 'OFFSHELL_STRATEGY should be < 0'
           stop 1
        end if
     end do
  end do
  amp_blob = - amp_blob ! 4 vertices, 3 propagators

contains

  subroutine init_decay_and_production_momenta ()
    k_decay_real = zero
    k_production = zero
    k_decay_real(:,4) = k(:,7)
    do i = 1, 6
       k_production(:,i) = k(:,i)
    end do
  end subroutine init_decay_and_production_momenta

  subroutine set_decay_and_production_momenta ()
     k_production(:,ass_quark(leg)) = k(:,ass_quark(leg)) + k(:,7)
     k_decay_real(:,3) = k(:,ass_quark(leg))
     k_decay_real(:,2) = k(:,ass_boson(leg))
     k_decay_real(:,1) = sum(k_decay_real,2)     !!! momentum conservation
     k_decay_born(:,3) = k(:,ass_quark(other_leg))
     k_decay_born(:,2) = k(:,ass_boson(other_leg))
     k_decay_born(:,1) = sum(k_decay_born,2)     !!! momentum conservation
     call set_production_momenta (k_production)
  end subroutine set_decay_and_production_momenta

end subroutine @ID@_compute_real

subroutine @ID@_threshold_get_amp_squared (amp2, p) bind(C)
  use iso_c_binding
  use kinds
  use opr_@ID@, full_proc_new_event => new_event
  use opr_@ID@, full_proc_get_amplitude => get_amplitude
  use opr_@ID@, full_proc_number_spin_states => number_spin_states
  use opr_@ID@, full_proc_number_particles_out => number_particles_out
  use @ID@_threshold
  use parameters_sm_tt_threshold
  use ttv_formfactors
  use constants
  implicit none
  real(c_default_float), intent(out) :: amp2
  real(default), dimension(0:3) :: amp2_array
  real(c_default_float), dimension(0:3,*), intent(in) :: p
  integer, dimension(0:3) :: signs
  logical :: real_computation
  integer :: i, hi
  amp_tree = zero
  real_computation = full_proc_number_particles_out () == 5
  if (real_computation) then
     USE_FF = .true.
     call @ID@_compute_real (amp2, p)
  else
     USE_FF = .false.
     call full_proc_new_event (p)
     do hi = 1, full_proc_number_spin_states()
        amp_tree(hi) = full_proc_get_amplitude (1, hi, 1)
     end do
     USE_FF = .true.
     call compute_born (p)
     select case (FF)
     case (EXPANDED_HARD_P0DEPENDENT, EXPANDED_HARD_P0CONSTANT, &
             EXPANDED_SOFT_P0CONSTANT, EXPANDED_SOFT_SWITCHOFF_P0CONSTANT, &
             EXPANDED_SOFT_HARD_P0CONSTANT)
        amp2 = expanded_amp2 (amp_tree, amp_blob(:,0))
     case (MATCHED)
        signs(0:3) = [+1, -1, +1, -1]
        amp2_array(0) = real (sum ((amp_tree + amp_blob(:,0)) * &
             conjg (amp_tree + amp_blob(:,0))))
        do i = 1, 3
           amp2_array(i) = expanded_amp2 (amp_tree, amp_blob(:,i))
        end do
        amp2 = sum (signs * amp2_array)
     case default
        amp2 = real (sum ((amp_tree + amp_blob(:,0)) * &
             conjg (amp_tree + amp_blob(:,0))))
     end select
     amp2 = amp2 * N_ / 4.0_default
  end if
end subroutine @ID@_threshold_get_amp_squared
