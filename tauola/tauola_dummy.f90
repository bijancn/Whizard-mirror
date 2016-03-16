subroutine ilc_tauola_init_call
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine ilc_tauola_init_call

subroutine ilc_tauola_get_helicity (itau, the_helicity)
  integer, intent(in)  :: itau
  integer, intent(out) :: the_helicity
  write (0, "(A)")  "**************************************************************"
  write (0, "(A)")  "*** Error: PYTHIA6 has not been enabled, WHIZARD terminates ***"
  write (0, "(A)")  "**************************************************************"
  stop
end subroutine ilc_tauola_get_helicity
