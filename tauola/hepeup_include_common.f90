module hepeup_include_common

  !use kinds, only: double 

  implicit none

  integer, parameter           ::  maxnup=500
  integer                      ::   nup
  integer                      ::   idprup
  !real(kind=double)                       :: xwgtup
  !real(kind=double)                       :: scalup
  !real(kind=double)                       :: aqedup
  !real(kind=double)                       :: aqcdup
  real                       :: xwgtup
  real                       :: scalup
  real                       :: aqedup
  real                       :: aqcdup
  integer, dimension(maxnup)   ::   idup
  integer, dimension(maxnup)   ::   istup
  integer, dimension(2,maxnup) ::   mothup
  integer, dimension(2,maxnup) ::   icolup 
  !real(kind=double), dimension(5,maxnup)  :: pup
  !real(kind=double), dimension(maxnup)    :: vtimup
  !real(kind=double), dimension(maxnup)    :: spinup
  real, dimension(5,maxnup)  :: pup
  real, dimension(maxnup)    :: vtimup
  real, dimension(maxnup)    :: spinup
  common/hepeup/nup,idprup,xwgtup,scalup,aqedup,aqcdup,idup,istup,mothup,icolup,pup,vtimup,spinup

end module hepeup_include_common


