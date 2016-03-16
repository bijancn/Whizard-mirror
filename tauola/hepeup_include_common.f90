module hepeup_include_common
  implicit none
  integer, parameter           ::  maxnup=500
  integer                      ::   nup
  integer                      ::   idprup
  double precision       :: xwgtup
  double precision       :: scalup
  double precision       :: aqedup
  double precision       :: aqcdup
  integer, dimension(maxnup)   ::   idup
  integer, dimension(maxnup)   ::   istup
  integer, dimension(2,maxnup) ::   mothup
  integer, dimension(2,maxnup) ::   icolup
  double precision, dimension(5,maxnup)  :: pup
  double precision, dimension(maxnup)    :: vtimup
  double precision, dimension(maxnup)    :: spinup
  common/hepeup/nup,idprup,xwgtup,scalup,aqedup,aqcdup,idup,istup,mothup,icolup,pup,vtimup,spinup
end module hepeup_include_common
