!> LMGC90 python wrap of soe:
!> \author F. Dubois
!> \date   April 2011
!>

module wrap_soe

  use ISO_C_BINDING

  !use soe_parameters

  use soe, only : set_matrix_storage, &
                  clean_module

  implicit none

  public cleanMemory

  contains

  subroutine cleanMemory() bind(c, name='soe_cleanMemory')
    implicit none

    call clean_module
  end subroutine

  subroutine SetStorage(cvalue_c) bind(c, name='soe_setStorage')
    implicit none
    character(c_char),intent(in), dimension(8) :: cvalue_c
    !
    character(len=8) :: cvalue
    integer :: i

    cvalue = ''
    do i=1,8
      cvalue = cvalue(1:i-1) // cvalue_c(i)
    end do

    call set_matrix_storage(trim(cvalue))

  end subroutine

end module
