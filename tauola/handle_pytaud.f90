subroutine handle_pytaud(itau,iorig,kforig,ndecay)
  use ilc_tauola_mod2
  implicit none
  integer, intent(in)  :: itau
  integer, intent(in)  :: iorig
  integer, intent(in)  :: kforig
  integer, intent(out) :: ndecay

  !


  call ilc_tauola_pytaud(itau,iorig,kforig,ndecay)

 
  return
end subroutine handle_pytaud


