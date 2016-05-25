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

function wthiggs (ifpseudo, hh1, hh2)
  double precision, dimension(4), intent(in) :: hh1, hh2
  logical, intent(in) :: ifpseudo
  double precision :: wthiggs
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end function wthiggs
  
subroutine taupi0 (mode, jak, ion)
  integer, intent(in) :: mode, jak
  integer, dimension(3), intent(in) :: ion
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine taupi0

subroutine photos (id)
  integer, intent(in) :: id
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine photos

subroutine ranmar (rvec, lenv)
  double precision, dimension(lenv) :: rvec
  integer, intent(in) :: lenv
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: TAUOLA has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine ranmar
