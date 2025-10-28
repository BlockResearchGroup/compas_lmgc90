MODULE wrap_utilities

  USE ISO_C_BINDING

  use utilities

  implicit none

  contains

  subroutine LogMessage(message, length) bind(c, name='utilities_logMes')
   implicit none
   type(c_ptr),    intent(in), value :: message
   integer(c_int), intent(in), value :: length
   !
   integer(kind=4) :: i
   character, dimension(:), pointer :: char_array
   character(len=256) :: f_message

   call c_f_pointer(cptr=message, fptr=char_array, shape=(/length/))

   f_message = ''
   do i = 1, min(length, 256)
     f_message = f_message(1:i-1) // char_array(i)
   end do
 
   if( length > 256 ) call logmes('[wrap_utitilities::LogMessage] : message too long')
   call logmes(f_message)

  end subroutine

  subroutine DisableLogMes() bind(c, name='utilities_DisableLogMes')
   implicit none

   call disable_logmes
 
  end subroutine

  subroutine EnableLogMes() bind(c, name='utilities_EnableLogMes')
   implicit none

   call enable_logmes
 
  end subroutine

  subroutine setIoUnitLimits(min_u, max_u) bind(c, name='utilities_setIoUnitLimits')
    implicit none
    integer(c_int), intent(in), value :: min_u, max_u

    call set_io_unit_limits(min_u, max_u)
  end subroutine

  subroutine setStopMode(to_stop) bind(c, name='utilities_setStopMode')
    implicit none
    logical(c_bool), intent(in), value :: to_stop

    no_stop = .not. to_stop

  end subroutine

  subroutine resetFatal() bind(c, name='utilities_resetFatal')
    implicit none
    
    call reset_fatal()

  end subroutine

  function checkFatal(mess) bind(c, name='utilities_checkFatal')
    implicit none
    type(c_ptr) :: mess
    integer(c_int) :: checkFatal
    !
    character(len=256) :: err
    character(len=1), dimension(:), pointer :: str
    integer(kind=4) :: i
    
    checkFatal = check_fatal(err)

    mess = c_null_ptr

    if( checkFatal > 0 ) then
      allocate(str(checkFatal+1))
      do i = 1, checkFatal
        str(i) = err(i:i)
      end do
      str(checkFatal+1) = c_null_char
      mess = c_loc(str(1))
    end if
  
  end function
  
  subroutine OpenFileStandardOutput(filename, length) bind(c, name='utilities_OpenFileStandardOutput')
    implicit none
    type(c_ptr),    intent(in), value :: filename
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    character, dimension(:), pointer :: char_array
    character(len=256) :: f_filename

    call c_f_pointer(cptr=filename, fptr=char_array, shape=(/length/))

    f_filename = ''
    do i = 1, min(length, 256)
       f_filename = f_filename(1:i-1) // char_array(i)
    end do
 
    if( length > 256 ) call logmes('[wrap_utitilities::OpenFileStandardOutput] : filename too long')

    call open_file_standard_output(f_filename)

  end subroutine
  
  subroutine CloseFileStandardOutput() bind(c, name='utilities_CloseFileStandardOutput')
    implicit none

    call close_file_standard_output()

  end subroutine

  subroutine initRandomSeed(seed, length) bind(c, name='utilities_InitRandomSeed')
    implicit none
    type(c_ptr)   , intent(in), value :: seed
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4), dimension(:), pointer :: s

    if( c_associated(seed) ) then

      call c_f_pointer(cptr=seed, fptr=s, shape=(/length/))
      call init_random_seed(s)

    else

      call init_random_seed()

    end if

  end subroutine initRandomSeed

  subroutine finalize() bind(c, name='utilities_Finalize')
    implicit none

    call close_all_io_unit()

  end subroutine finalize

end module
