MODULE wrap_ExternalModels

  !!****h* LMGC90.CHIPY/EXTERNALMODELS
  !! NAME
  !!  module wrap_ExternalModels
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/EXTERNALMODELS
  !!****


  use ExternalModels
  implicit none
  contains
  subroutine InitModels
   implicit none

   call init_external_Models
 
  end subroutine
  subroutine CheckProperties
   implicit none

   call check_external_ppset
 
  end subroutine
end module
