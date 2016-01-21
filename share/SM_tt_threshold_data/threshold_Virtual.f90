module @ID@_virtual
  use iso_c_binding
  use kinds
  use openloops
  use parameters_sm_tt_threshold
  implicit none
  integer, dimension(4) :: id
end module @ID@_virtual

subroutine @ID@_start_openloops () bind(C)
  use @ID@_virtual
  integer, dimension(12), parameter :: pdgs = [1,2,3,4,5,6,11,13,15,23,24,25]
  character(32) :: buffer
  integer :: i
  ! coupling order alpha_ew^1, implies QCD correction for loop process
  call set_parameter("order_ew", 1)
  !call set_parameter("order_ew", 4)
  !call set_parameter("order_qcd", 1)

  call set_parameter("model", "sm")

  do i = 1, size(pdgs)
     if (pdgs(i) > 4) then
        write (buffer, "(I0)")  pdgs(i)
        call set_parameter("mass(" // trim (adjustl (buffer)) // ")", mass(pdgs(i)))
        call set_parameter("width(" // trim (adjustl (buffer)) // ")", width(pdgs(i)))
     end if
  end do
  call set_parameter("alpha", real(1 / alphaemi, kind=double))

  ! Increase verbosity level to list loaded libraries
  call set_parameter("verbose", 3)

  call set_parameter("psp_tolerance", 10e-7_double)
  call set_parameter("use_cms", 0)
  call set_parameter("me_cache", 0)

  ! 1 for tree-like matrix elements (tree, color and spin correlations),
  ! 11 for loop, 12 for loop^2
  id(1) = register_process("6(-1) -> 24 5", 11)
  id(2) = register_process("6(+1) -> 24 5", 11)
  id(3) = register_process("-6(-1) -> -24 -5", 11)
  id(4) = register_process("-6(+1) -> -24 -5", 11)
  !id(1) = register_process("11 -11 -> 24 -24 5 -5", 11)
  !id(2) = 0

  call start()

end subroutine @ID@_start_openloops

subroutine @ID@_olp_eval2 (i_flv, alpha_s_c, parray, mu_c, &
       sqme_c, acc_c) bind(C)
  use @ID@_threshold
  use @ID@_virtual
  implicit none
  integer(c_int), intent(in) :: i_flv
  real(c_default_float), intent(in) :: alpha_s_c
  real(c_default_float), dimension(0:3,*), intent(in) :: parray
  real(c_default_float), intent(in) :: mu_c
  real(c_default_float), dimension(4), intent(out) :: sqme_c
  real(c_default_float), intent(out) :: acc_c
  integer :: h_el, h_pos, h_t, h_tbar
  integer :: i, leg, other_leg, ffi, this_id
  real(double) :: m2_tree, m2_loop(0:2), acc
  real(double) :: p_decay(0:3,4)
  real(double) :: mu, alpha_s, virtual_decay_me
  call msg_debug (D_ME_METHODS, "@ID@_olp_eval2")
  if (i_flv /= 1)  call msg_fatal ("i_flv /= 1, threshold interface was not built for this")
  if (any (id <= 0))  call msg_fatal ("Could not register process in OpenLoops")
  if (OFFSHELL_STRATEGY >= 0)  call msg_fatal ("OFFSHELL_STRATEGY should be < 0")
  alpha_s = alpha_s_c
  mu = mu_c
  call init_workspace ()
  call set_parameter("alpha_s", alpha_s)
  call set_parameter("mu", mu)
  total_m2_loop = 0
  total_m2_tree = 0
  do leg = 1, 2
     other_leg = 3 - leg
     call set_decay_and_production_momenta ()
     do h_el = -1, 1, 2  do h_pos = -1, 1, 2
        call compute_owfs ([h_el, h_pos])
           do ffi = 0, ffi_end
              do h_t = -1, 1, 2
                 do h_tbar = -1, 1, 2
                    production_me = calculate_blob (ffi, h_t, h_tbar)
                    if (leg == 1) then
                       born_decay_me = anti_top_decay_born (h_tbar)
                       this_id = (3 + h_t) / 2
                    else
                       born_decay_me = top_decay_born (h_t)
                       this_id = (3 + h_t) / 2 + 2
                    end if
    dynamic_top_mass = sqrt (p1 * p1)
    top_width = ttv_wtpole (p1*p1, ff_modes(ffi))
                    call set_parameter("mass(" // trim (adjustl (buffer)) // ")", mass(pdgs(i)))
                    call set_parameter("width(" // trim (adjustl (buffer)) // ")", width(pdgs(i)))
                    call evaluate_loop(id(this_id), p_decay, m2_tree, m2_loop, acc)
                    ! i guess its summed over color but not divided by N_ ?
                    virtual_decay_me = virtual_decay_me / N_
                    total_m2_loop = total_m2_loop + production_me * &
                          virtual_decay_me * born_decay_me * top_propagators (ffi)
                    total_m2_tree = total_m2_tree + production_me * &
                          virtual_decay_me * born_decay_me * top_propagators (ffi)
                    call debug_computation_status ()
                 end do
              end do
           end do
     end do
  end do
  sqme_c = [total_m2_loop(2), total_m2_loop(1), total_m2_loop(0), total_m2_tree]

end subroutine @ID@_olp_eval2

subroutine @ID@_stop_openloops () bind(C)
  use iso_c_binding
  use openloops
  call finish()
end subroutine @ID@_stop_openloops
