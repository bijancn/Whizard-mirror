subroutine dekay (kto, hx)
  integer, intent(in) :: kto
  double precision, dimension(4), intent(in) :: hx
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine dekay

subroutine dexay (kto, pol)
  integer, intent(in) :: kto
  double precision, dimension(4), intent(in) :: pol
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine dexay

subroutine tauola (mode, keypol)
  integer, intent(in) :: mode, keypol
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine tauola
