MODULE wrap_ExternalModels

  USE ISO_C_BINDING

  use ExternalModelsHandler

  implicit none

  contains

  subroutine InitModels() bind(c, name='ExternalModels_InitModels')
   implicit none

   call init_external_Models
 
  end subroutine

  subroutine StoreProperties() bind(c, name='ExternalModels_StoreProperties')
   implicit none

   call store_external_ppset
 
  end subroutine

  subroutine CleanMemory() bind(c, name='ExternalModels_CleanMemory')
    implicit none

    call clean_memory

  end subroutine

end module
