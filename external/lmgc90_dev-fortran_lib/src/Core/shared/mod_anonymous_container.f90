
!> anonymous database : management
!> \author R. Mozul
!> \date   March 2011
!>

module anonymous_container

  use utilities, only : faterr

  use anonymous, only : list_data    => T_object      , &
                        new_data     => new_object    , &
                        erase_data   => erase_object  , &
                        close_data   => close_object  , &
                        open_data    => open_object   , &
                        copy_data    => copy_object   , &
                        display_data => display_object, &
                        new_object, &
                        set_rank  , &
                        get_rank  , &
                        set_c5_ptr, &
                        set_i4_ptr, &
                        set_r8_ptr, &
                        set_cx_ptr

  implicit none

  private

  include 'container_type.f90'

  public add_object_to_container, &
         display_object_container

  contains

  include 'container_methods.f90'

  !> \brief add an object to object container
  subroutine add_object_to_container(objects, rank, c5, i4, r8, cx)
    implicit none
    type(container), intent(inout) :: objects !< [in,out] object container
    integer(kind=4), intent(in)    :: rank    !< [in] rank of the new object
    character(len=5),   dimension(:), pointer :: c5 !< [in] short string data of the object
    integer(kind=4),    dimension(:), pointer :: i4 !< [in] integer data of the object
    real(kind=8),       dimension(:), pointer :: r8 !< [in] real data of the object
    character(len=128), dimension(:), pointer :: cx !< [in] string data of the object
    !
    type(list_data), pointer :: object
   
    object => null()

    if( objects%open ) then
      object => new_object()
      call set_rank(object, rank)
      call set_c5_ptr(object, c5)
      call set_i4_ptr(object, i4)
      call set_r8_ptr(object, r8)
      call set_cx_ptr(object, cx)

      call add_to_container(objects, object)
    else
      call faterr('anonymous_container::add_object_to_container','object container close')
    end if

  end subroutine

  !> \brief display an object container
  subroutine display_object_container(objects, ifich)
    implicit none
    type(container), intent(in) :: objects !< [in] object container
    integer(kind=4), optional   :: ifich   !< [in] (optional) unit number in which to write
    !
    integer(kind=4) :: i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) '            number of objects : ', get_nb_data(objects)
    call display_container(objects, ifich)

  end subroutine

end module

