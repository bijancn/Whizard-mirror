module @ID@_virtual
  implicit none
  integer, dimension(2) :: id
contains
end module @ID@_virtual

subroutine @ID@_start_openloops () bind(C)
  use iso_c_binding
  use kinds
  use @ID@_virtual
  use openloops
  real(8) :: mZ = 91.2
  ! coupling order alpha_ew^1, implies QCD correction for loop process
  call set_parameter("order_ew", 1)

  ! set Z mass
  call set_parameter("mass(23)", mZ)

  ! Increase verbosity level to list loaded libraries
  call set_parameter("verbose", 1)

  ! register one-loop amplitude for process d dbar -> Z u ubar
  ! The "ppzjj" process library must be installed before via
  ! $ ./scons auto=ppzjj
  !
  ! second argument of register_process:
  ! 1 for tree-like matrix elements (tree, color and spin correlations),
  ! 11 for loop, 12 for loop^2
  id(1) = register_process("6 -> 24 5", 11)
  id(2) = register_process("-6 -> -24 -5", 11)
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
  alpha_s = alpha_s_c
  mu = mu_c
  do i = 1, 6
     p_ex(:,i) = parray(:,i)
  end do
  do i = 1, 2
     if (id(i) > 0) then
        call set_parameter("alpha_s", alpha_s)
        call set_parameter("mu", mu)

        do k = 1, size(p_ex,2)
          print *, 'P[', int(k,1), '] =', p_ex(:,k)
        end do

        call evaluate_tree(id(i), p_ex, m2_tree)
        print *
        print *, "evaluate_tree"
        print *, "Tree:       ", m2_tree

        call evaluate_loop(id(i), p_ex, m2_tree, m2_loop(0:2), acc)
        print *
        print *, "evaluate_loop"
        print *, "Tree:       ", m2_tree
        print *, "Loop ep^0:  ", m2_loop(0)
        print *, "Loop ep^-1: ", m2_loop(1)
        print *, "Loop ep^-2: ", m2_loop(2)
        print *, "accuracy:   ", acc
        print *
     else
        print *, "Could not load process ", id(i)
        stop 1
     end if
  end do

end subroutine @ID@_olp_eval2

subroutine @ID@_stop_openloops () bind(C)
  use iso_c_binding
  use openloops
  call finish()
end subroutine @ID@_stop_openloops
