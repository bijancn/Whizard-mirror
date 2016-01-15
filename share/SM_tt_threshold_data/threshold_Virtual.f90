module @ID@_virtual
  use iso_c_binding
  use kinds
  use openloops
  use parameters_sm_tt_threshold
  implicit none
  integer, dimension(2) :: id
contains
end module @ID@_virtual

subroutine @ID@_start_openloops () bind(C)
  use @ID@_virtual
  integer, dimension(12), parameter :: pdgs = [1,2,3,4,5,6,11,13,15,23,24,25]
  character(32) :: buffer
  integer :: i
  ! coupling order alpha_ew^1, implies QCD correction for loop process
  !call set_parameter("order_ew", 1)
  call set_parameter("order_ew", 4)
  !call set_parameter("order_qcd", 1)

  call set_parameter("model", "sm")

  do i = 1, size(pdgs)
     if (pdgs(i) > 4) then
        write (buffer, "(I0)")  pdgs(i)
        call set_parameter("mass(" // trim (adjustl (buffer)) // ")", mass(pdgs(i)))
        call set_parameter("width(" // trim (adjustl (buffer)) // ")", width(pdgs(i)))
     end if
  end do
  call set_parameter("alpha", one / alphaemi)

  ! Increase verbosity level to list loaded libraries
  call set_parameter("verbose", 3)

  call set_parameter("psp_tolerance", 10e-7_double)
  call set_parameter("use_cms", 0)
  call set_parameter("me_cache", 0)

  ! register one-loop amplitude for process d dbar -> Z u ubar
  ! The "ppzjj" process library must be installed before via
  ! $ ./scons auto=ppzjj
  !
  ! second argument of register_process:
  ! 1 for tree-like matrix elements (tree, color and spin correlations),
  ! 11 for loop, 12 for loop^2
  !id(1) = register_process("6(-1) -> 24 5", 11)
  !id(2) = register_process("6(+1) -> 24 5", 11)
  id(2) = 0
  id(1) = register_process("11 -11 -> 24 -24 5 -5", 11)
  !id(2) = register_process("-6 -> -24 -5", 11)
  !id(1) = register_process("6 -> 24 5", 11)

  call start()

end subroutine @ID@_start_openloops

subroutine @ID@_olp_eval2 (i_flv, alpha_s_c, parray, mu_c, &
       sqme_c, acc_c) bind(C)
  use iso_c_binding
  use kinds
  use openloops
  use @ID@_threshold
  use @ID@_virtual
  implicit none
  integer(c_int), intent(in) :: i_flv
  real(c_default_float), intent(in) :: alpha_s_c
  real(c_default_float), dimension(0:3,*), intent(in) :: parray
  real(c_default_float), intent(in) :: mu_c
  real(c_default_float), dimension(4), intent(out) :: sqme_c
  real(c_default_float), intent(out) :: acc_c
  integer :: k, i
  real(double) :: m2_tree, m2_loop(0:2), acc
  real(double) :: p_ex(0:3,6)
  real(double) :: mu, alpha_s
  if (i_flv /= 1) then
     print *, 'i_flv /= 1, threshold interface was not built for this'
     stop 1
  end if
  alpha_s = alpha_s_c
  mu = mu_c
  do i = 1, 6
     p_ex(:,i) = parray(:,i)
  end do
  do i = 1, 2
     if (id(i) > 0) then
        call set_parameter("alpha_s", alpha_s)
        call set_parameter("mu", mu)
        call evaluate_loop(id(i), p_ex, m2_tree, m2_loop(0:2), acc)
     else
        !print *, "Could not load process ", id(i)
        !stop 1
     end if
  end do
  sqme_c = [m2_loop(2), m2_loop(1), m2_loop(0), m2_tree]

end subroutine @ID@_olp_eval2

subroutine @ID@_stop_openloops () bind(C)
  use iso_c_binding
  use openloops
  call finish()
end subroutine @ID@_stop_openloops
