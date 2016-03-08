module momdec_include_common

  !use kinds, only: double 

  implicit none

  real, dimension(4) :: q1
  real, dimension(4) :: q2
  real, dimension(4) :: p1
  real, dimension(4) :: p2
  real, dimension(4) :: p3
  real, dimension(4) :: p4
  !real(kind=double), dimension(4) :: q1
  !real(kind=double), dimension(4) :: q2
  !real(kind=double), dimension(4) :: p1
  !real(kind=double), dimension(4) :: p2
  !real(kind=double), dimension(4) :: p3
  !real(kind=double), dimension(4) :: p4

  common/momdec/q1,q2,p1,p2,p3,p4

end module momdec_include_common


