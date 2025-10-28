! Warning: some of the following comments respect doxygen formats

!> anonymous object
!> \author F. Dubois
!> \date   March 2009
!>

module anonymous

  implicit none

  private

  !> a generic object 
  !> \warning it uses pointer and not allocatable array
  type, public :: T_object
    private
    integer(kind=4) :: rank !< a rank
    character(len=5), dimension(:), pointer :: c5_vector => null()
    integer(kind=4),  dimension(:), pointer :: i4_vector => null()
    real(kind=8),     dimension(:), pointer :: r8_vector => null()
    !
    ! rm : bon c'est un peu moisaille tout ca, mais pour les conditions aux limites
    ! j'ai besoin de pouvoir stocker des noms de fichier... du coup le moindre
    ! cout en terme de developpement c'etait d'ajouter un vecteur de grandes
    ! chaine de caracteres.
    character(len=128), dimension(:), pointer :: cx_vector => null()
  end type T_object

  public new_object   , erase_object     , &
         close_object , open_object      , &
         copy_object                     , &
         set_rank     , get_rank         , &
         set_c5_vector, set_c5_ptr       , &
         set_r8_vector, set_r8_ptr       , &
         set_i4_vector, set_i4_ptr       , &
         set_cx_vector, set_cx_ptr       , &
         get_c5_vector, nullify_c5_vector, &
         get_r8_vector, nullify_r8_vector, &
         get_i4_vector, nullify_i4_vector, &
         get_cx_vector, nullify_cx_vector, &
         display_object


  contains

  !> \brief object constructor \n 
  !> default or copy 
  function new_object(other_object)
    implicit none
    type(T_object), optional, intent(in) :: other_object !< [in] (optional) object to copy from
    type(T_object), pointer              :: new_object   !< [return] pointer on a new object
 
    ! allocation of the returned object
    new_object => null()
    allocate(new_object)
      
    if (present(other_object)) then
      new_object%rank = other_object%rank
      if(associated(other_object%c5_vector)) call set_c5_vector(new_object, other_object%c5_vector)
      if(associated(other_object%i4_vector)) call set_i4_vector(new_object, other_object%i4_vector)
      if(associated(other_object%r8_vector)) call set_r8_vector(new_object, other_object%r8_vector)
      if(associated(other_object%cx_vector)) call set_cx_vector(new_object, other_object%cx_vector)
    else
      new_object%rank = 0
      new_object%c5_vector => null()
      new_object%r8_vector => null()
      new_object%i4_vector => null()
      new_object%cx_vector => null()
    endif
  end function

  !> \brief erase an object
  subroutine erase_object(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] erase content of an object

    object%rank = 0

    call nullify_c5_vector(object)
    call nullify_i4_vector(object)
    call nullify_r8_vector(object)
    call nullify_cx_vector(object)

  end subroutine
  
  !> \brief copy from an object to an other
  subroutine copy_object(new_object, other_object)
    implicit none
    type(T_object), intent(out) :: new_object   !< [in] object copy to
    type(T_object), intent(in)  :: other_object !< [in] object copy from

    new_object%rank = other_object%rank
    if(associated(other_object%c5_vector)) call set_c5_vector(new_object, other_object%c5_vector)
    if(associated(other_object%i4_vector)) call set_i4_vector(new_object, other_object%i4_vector)
    if(associated(other_object%r8_vector)) call set_r8_vector(new_object, other_object%r8_vector)
    if(associated(other_object%cx_vector)) call set_cx_vector(new_object, other_object%cx_vector)

  end subroutine

  !> \brief close an object
  subroutine close_object(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] object open in input, close on output

    ! nothing to do here, but the subroutine must exist
    ! since it is used in linkedlist.f90 and thus in anonymous_container

  end subroutine

  !> \brief open an object
  subroutine open_object(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] object close in input, open on output

    ! nothing to do here, but the subroutine must exist
    ! since it is used in linkedlist.f90 and thus in anonymous_container

  end subroutine

  ! ***

  !> \brief set rank of an object
  subroutine set_rank(object,rank)
    implicit none
    type(T_object),  intent(inout) :: object !< [in, out] object
    integer(kind=4), intent(in)    :: rank   !< [in] new rank

    object%rank = rank

  end subroutine

  !> \brief get rank of an object
  function get_rank(object)
    implicit none
    type(T_object), intent(in) :: object   !< [in] object
    integer(kind=4)            :: get_rank !< [return] rank of object

    get_rank = object%rank

  end function

  ! ***

  !> \brief set c5_vector field of an object
  !> allocation of c5_vector field if needed and copy
  subroutine set_c5_vector(object,c5_vector)
    implicit none
    type(T_object), intent(inout)              :: object    !< [in,out] object
    character(len=5), dimension(:), intent(in) :: c5_vector !< [in] new c5_vector
    !
    integer(kind=4) :: c5_size

    c5_size = size(c5_vector)
    if( c5_size == 0 ) then
      call nullify_c5_vector(object)
      return
    end if

    if (associated(object%c5_vector)) then
      if (size(object%c5_vector) /= c5_size ) then
        deallocate(object%c5_vector)
        allocate(object%c5_vector(c5_size))
      endif
    else
      allocate(object%c5_vector(c5_size))
    endif

    object%c5_vector(1:c5_size) = c5_vector(1:c5_size)

  end subroutine

  !> \brief set c5_vector field of an object
  !> association of pointer and no allocation/copy
  subroutine set_c5_ptr(object,c5_vector)
    implicit none
    type(T_object), intent(inout)           :: object    !< [in,out] object
    character(len=5), dimension(:), pointer :: c5_vector !< [in] c5_vector

    if (associated(object%c5_vector)) call nullify_c5_vector(object)

    object%c5_vector => c5_vector

  end subroutine

  !> \brief nullify c5_vector field of an object
  !> free allocated memory
  subroutine nullify_c5_vector(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] object

    if (associated(object%c5_vector)) deallocate(object%c5_vector)
    object%c5_vector => null()

  end subroutine

  !> \brief get a pointer on c5_vector field of an object
  function get_c5_vector(object)
    implicit none
    type(T_object), intent(inout)           :: object        !< [in] object
    character(len=5), dimension(:), pointer :: get_c5_vector !< [return] pointer on c5_vector

    get_c5_vector => object%c5_vector

  end function

  !> \brief set i4_vector field of an object
  !> allocation of i4_vector field if needed and copy
  subroutine set_i4_vector(object,i4_vector)
    implicit none
    type(T_object), intent(inout)             :: object    !< [in,out] object
    integer(kind=4), dimension(:), intent(in) :: i4_vector !< [in] new i4_vector
    !
    integer(kind=4) :: i4_size

    i4_size = size(i4_vector)
    if( i4_size == 0 ) then
      call nullify_i4_vector(object)
      return
    end if

    if (associated(object%i4_vector)) then
      if (size(object%i4_vector) /= i4_size ) then
        deallocate(object%i4_vector)
        allocate(object%i4_vector(i4_size))
      endif
    else
      allocate(object%i4_vector(i4_size))
    endif

    object%i4_vector(1:i4_size) = i4_vector(1:i4_size)

  end subroutine

  !> \brief set i4_vector field of an object
  !> association of pointer and no allocation/copy
  subroutine set_i4_ptr(object,i4_vector)
    implicit none
    type(T_object), intent(inout)          :: object    !< [in,out] object
    integer(kind=4), dimension(:), pointer :: i4_vector !< [in] i4_vector

    if (associated(object%i4_vector)) call nullify_i4_vector(object)

    object%i4_vector => i4_vector

  end subroutine

  !> \brief nullify i4_vector field of an object
  !> free allocated memory
  subroutine nullify_i4_vector(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] object

    if (associated(object%i4_vector)) deallocate(object%i4_vector)
    object%i4_vector => null()

  end subroutine

  !> \brief get a pointer on i4_vector field of an object
  function get_i4_vector(object)
    implicit none
    type(T_object), intent(inout)          :: object        !< [in] object
    integer(kind=4), dimension(:), pointer :: get_i4_vector !< [return] pointer on i4_vector

    get_i4_vector => object%i4_vector

  end function

  !> \brief set r8_vector field of an object
  !> allocation of r8_vector field if needed and copy
  subroutine set_r8_vector(object,r8_vector)
    implicit none
    type(T_object), intent(inout)          :: object    !< [in,out] object
    real(kind=8), dimension(:), intent(in) :: r8_vector !< [in] new r8_vector
    !
    integer(kind=4) :: r8_size

    r8_size = size(r8_vector)
    if( r8_size == 0 ) then
      call nullify_r8_vector(object)
      return
    end if

    if (associated(object%r8_vector)) then
      if (size(object%r8_vector) /= r8_size ) then
        deallocate(object%r8_vector)
        allocate(object%r8_vector(r8_size))
      endif
    else
      allocate(object%r8_vector(r8_size))
    endif

    object%r8_vector(1:r8_size) = r8_vector(1:r8_size)

  end subroutine

  !> \brief set r8_vector field of an object
  !> association of pointer and no allocation/copy
  subroutine set_r8_ptr(object,r8_vector)
    implicit none
    type(T_object), intent(inout)       :: object    !< [in,out] object
    real(kind=8), dimension(:), pointer :: r8_vector !< [in] r8_vector

    if (associated(object%r8_vector)) call nullify_r8_vector(object)

    object%r8_vector => r8_vector

  end subroutine

  !> \brief nullify r8_vector field of an object
  !> free allocated memory
  subroutine nullify_r8_vector(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] object

    if (associated(object%r8_vector)) deallocate(object%r8_vector)
    object%r8_vector => null()

  end subroutine

  !> \brief get a pointer on r8_vector field of an object
  function get_r8_vector(object)
    implicit none
    type(T_object), intent(inout)       :: object        !< [in] object
    real(kind=8), dimension(:), pointer :: get_r8_vector !< [return] pointer on r8_vector

    get_r8_vector => object%r8_vector

  end function

  !> \brief set cx_vector field of an object
  !> allocation of cx_vector field if needed and copy
  subroutine set_cx_vector(object,cx_vector)
    implicit none
    type(T_object), intent(inout)                :: object    !< [in,out] object
    character(len=128), dimension(:), intent(in) :: cx_vector !< [in] new cx_vector
    !
    integer(kind=4) :: cx_size

    cx_size = size(cx_vector)
    if( cx_size == 0 ) then
      call nullify_cx_vector(object)
      return
    end if

    if (associated(object%cx_vector)) then
      if (size(object%cx_vector) /= cx_size ) then
        deallocate(object%cx_vector)
        allocate(object%cx_vector(cx_size))
      endif
    else
      allocate(object%cx_vector(cx_size))
    endif

    object%cx_vector(1:cx_size) = cx_vector(1:cx_size)

  end subroutine

  !> \brief set cx_vector field of an object
  !> association of pointer and no allocation/copy
  subroutine set_cx_ptr(object,cx_vector)
    implicit none
    type(T_object), intent(inout)             :: object    !< [in,out] object
    character(len=128), dimension(:), pointer :: cx_vector !< [in] cx_vector

    if (associated(object%cx_vector)) call nullify_cx_vector(object)

    object%cx_vector => cx_vector

  end subroutine

  !> \brief nullify cx_vector field of an object
  !> free allocated memory
  subroutine nullify_cx_vector(object)
    implicit none
    type(T_object), intent(inout) :: object !< [in,out] object

    if (associated(object%cx_vector)) deallocate(object%cx_vector)
    object%cx_vector => null()

  end subroutine

  !> \brief get a pointer on cx_vector field of an object
  function get_cx_vector(object)
    implicit none
    type(T_object), intent(inout)             :: object        !< [in] object
    character(len=128), dimension(:), pointer :: get_cx_vector !< [return] pointer on cx_vector

    get_cx_vector => object%cx_vector

  end function

  ! ***

  !> \brief display object
  subroutine display_object(object, ifich)
    implicit none
    type(T_object), intent(in) :: object !< [in] object to display
    integer(kind=4), optional  :: ifich  !< [in] (optional) unit number in which to write
    !
    integer(kind=4) :: i, i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) '            object of rank : ', object%rank
    if( associated(object%c5_vector) ) write(i_unit,*) '            c5_vector : ', object%c5_vector
    if( associated(object%i4_vector) ) write(i_unit,*) '            i4_vector : ', object%i4_vector
    if( associated(object%r8_vector) ) write(i_unit,*) '            r8_vector : ', object%r8_vector
    if( associated(object%cx_vector) ) then
      write(i_unit,*) '            cx_vector : '
      do i = 1, size(object%cx_vector)
        write(i_unit,*) '                       - ', trim(object%cx_vector(i))
      end do
    end if

  end subroutine

end module anonymous
