module @ID@_virtual
  implicit none
  integer, dimension(2) :: id
contains
end module @ID@_virtual

subroutine @ID@_start_openloops () bind(C)
  use iso_c_binding
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

subroutine @ID@_compute_virtual() result (amp) bind(C)
  use iso_c_binding
  use openloops
  use @ID@_threshold
  implicit none
  complex(8) :: amp
  integer :: k, i
  real(8) :: m2_tree, m2_loop(0:2), acc
  real(8) :: p_ex(0:3,5)
  real(8) :: mu = 100, alpha_s = 0.1

  do i = 1, 2
     id = id(i)
     if (id > 0) then
        call set_parameter("alpha_s", alpha_s)
        call set_parameter("mu", mu)

        do k = 1, size(p_ex,2)
          print *, 'P[', int(k,1), '] =', p_ex(:,k)
        end do

        call evaluate_tree(id, p_ex, m2_tree)
        print *
        print *, "evaluate_tree"
        print *, "Tree:       ", m2_tree

        call evaluate_loop(id, p_ex, m2_tree, m2_loop(0:2), acc)
        print *
        print *, "evaluate_loop"
        print *, "Tree:       ", m2_tree
        print *, "Loop ep^0:  ", m2_loop(0)
        print *, "Loop ep^-1: ", m2_loop(1)
        print *, "Loop ep^-2: ", m2_loop(2)
        print *, "accuracy:   ", acc
        print *
     else
        print *, "Could not load process ", id
        stop 1
     end if
  end do

end subroutine @ID@_compute_virtual

subroutine @ID@_stop_openloops () bind(C)
  use iso_c_binding
  use openloops
  call finish()
end subroutine @ID@
