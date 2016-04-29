subroutine dekay (kto, hx)
  integer, intent(in) :: kto
  double precision, dimension(4), intent(in) :: hx
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine dekay
