MODULE wrap_utilities

  !!****h* LMGC90.CHIPY/UTILITIES
  !! NAME
  !!  module wrap_utilities
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/UTILITIES
  !!****


  use utilities
  implicit none
  contains
  subroutine DisableLogMes
   implicit none

   call disable_logmes
 
  end subroutine
end module
