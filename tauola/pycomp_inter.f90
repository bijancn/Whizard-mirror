      module pycomp_inter
!
      interface
      function pycomp(kf)
      implicit none
      integer                                 :: pycomp
      integer, intent(in)                     :: kf
      end function pycomp
      end interface
!
      end module pycomp_inter

