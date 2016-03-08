      module pyr_inter
!
      interface
      function pyr(i_dum)
      !use kinds, only: double
      implicit none
      !real(kind=double)                       :: pyr
      real                       :: pyr
      integer, intent(in)                     :: i_dum
      end function pyr
      end interface
!
      end module pyr_inter

