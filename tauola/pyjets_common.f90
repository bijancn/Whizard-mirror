module pyjets_common

  !use kinds, only: double 

  implicit none

  integer :: n
  integer :: npad
  integer, dimension(4000,5) :: k
  !real(kind=double), dimension(4000,5) :: p
  !real(kind=double), dimension(4000,5) :: v
  real, dimension(4000,5) :: p
  real, dimension(4000,5) :: v
  common/pyjets/n,npad,k,p,v


end module pyjets_common




