module openloops

  implicit none
  
  contains

  function register_process(process, amptype)
    character(len=*), intent(in) :: process
    integer, intent(in) :: amptype
    integer :: register_process
    call print_openloops_dummy_error_message ()
  end function register_process

  subroutine print_openloops_dummy_error_message ()
    write (*, "(A)") "********************************************************"
    write (*, "(A)") "*** OpenLoops: Error: Library is not linked ***"
    write (*, "(A)") "********************************************************"
    stop
  end subroutine print_openloops_dummy_error_message

end module openloops
