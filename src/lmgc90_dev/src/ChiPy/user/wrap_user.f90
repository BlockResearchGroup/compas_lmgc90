module wrap_user
  
  USE ISO_C_BINDING

  use user
  implicit none

  contains

  ! uncomment this line to manage externalFEM in the wrapper
  ! include "../../../../LMGC90v2_BindingExternalFEM/inc_wrap_user.f90"
  subroutine getWoodFrame(matrix_in, idim1, idim2, matrix_out, odim1, odim2, v1, i1, v2, i2) bind(c, name='user_getWoodFrame')
    implicit none
    type(c_ptr)   , value :: matrix_in
    integer(c_int), value :: idim1, idim2
    type(c_ptr)           :: matrix_out
    integer(c_int)        :: odim1, odim2
    type(c_ptr)   , value :: v1, v2
    integer(c_int), value :: i1, i2
    !
    integer :: space_dim
    real(kind=8), dimension(:,:,:), pointer :: wood_frame
    real(kind=8), dimension(:,:)  , pointer :: points
    real(kind=8), dimension(:)    , pointer :: center, orient

    if( idim2/=3 .or. i1/=3 .or. i2/=3 ) then
        print *, '[ERROR:user_getWoodFrame]: wrong shape of input array'
        stop
    end if

    allocate(wood_frame(3,3,idim1))
    call c_f_pointer(cptr=matrix_in, fptr=points, shape=(/3,idim1/))
    call c_f_pointer(cptr=v1       , fptr=center, shape=(/i1/))
    call c_f_pointer(cptr=v2       , fptr=orient, shape=(/i2/))

    call gp_ortho_frame(3, idim1, points(:,:), wood_frame, center, orient)

    matrix_out = c_loc(wood_frame(1,1,1))
    odim1 = 9
    odim2 = idim1

  end subroutine getWoodFrame

end module  
