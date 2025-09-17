
include 'linkedlist_methods.f90'

!!---------- To work on containers ---------- !!

!!---------- management ---------- !!

!> \brief add data to an open container
subroutine add_to_container(cont, data)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container in which to add
  type(LIST_DATA), pointer       :: data !< [in] data to add

  if( cont%open ) then
    if( cont%nb_data == 0 ) then
      call list_create(cont%root, data)
    else
      call list_insert_head(cont%root, data)
    end if
    cont%nb_data = cont%nb_data + 1
  else
    write(*,*) 'ERROR[add_to_container] : cannot add to a close container'
    stop 1
  end if

end subroutine

!> \brief close a container
subroutine close_container(cont)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container to close
  !
  type(LINKED_LIST), pointer :: root_too
  integer(kind=4) :: i_data

  if( cont%open ) then

    ! no reason this would ever happen
    if( allocated(cont%data_array) ) deallocate(cont%data_array)
    if( cont%nb_data /= list_count(cont%root) ) then
      write(*,*) 'ERROR[close_container] : this is really bad... wrong size within container data structure'
      stop 1
    end if
    ! done with stupid checks

    ! since we head insert in the list, we reverse insertion in the array
    ! to get an array in the same order than we insert them in the list
    allocate(cont%data_array(cont%nb_data))
    i_data = cont%nb_data
    do while( associated(cont%root) )
      call close_data(cont%root%data)
      call copy_data(cont%data_array(i_data), list_get_data(cont%root))
      root_too => cont%root
      call list_destroy_element(cont%root, root_too)
      i_data = i_data - 1
    end do
    !if( i_data /= cont%nb_data ) stop 1 !really too paranoid

    cont%open = .false.

  else
    write(*,*) 'WARNING[close_container] : container already close'
  end if

end subroutine

!> \brief open a container
subroutine open_container(cont)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container to open
  !
  type(LIST_DATA), pointer :: data
  integer(kind=4) :: i_data

  if( cont%open ) then
    write(*,*) 'WARNING[open_container] : container already open'
  else

    ! no reason this would ever happen
    if( associated(cont%root) ) call list_destroy(cont%root)
    if( cont%nb_data /= size(cont%data_array) ) then
      write(*,*) 'ERROR[open_container] : this is really bad... wrong size within container data structure'
      stop 1
    end if
    ! done with stupid checks

    if( cont%nb_data == 0 ) then
      cont%open = .true.
      return
    end if

    data => new_data(cont%data_array(1))
    call erase_data(cont%data_array(1))
    call list_create(cont%root, data)
    do i_data = 2, cont%nb_data
      data => new_data(cont%data_array(i_data))
      call erase_data(cont%data_array(i_data))
      call list_insert_head(cont%root, data)
      call open_data(cont%root%data)
    end do
    deallocate(cont%data_array)
    !if( i_data /= list_count(cont%root) ) stop 1 !really too paranoid

    cont%open = .true.

  end if

end subroutine

!> \brief erase a container
!> free any allocated memory within objects
subroutine erase_container(cont)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container to erase
  !
  integer(kind=4) :: i_data

  if( associated(cont%root) ) call list_destroy(cont%root)
  if( allocated(cont%data_array) ) then
    do i_data = 1, size(cont%data_array)
      call erase_data(cont%data_array(i_data))
    end do
    deallocate(cont%data_array)
  end if

  cont%nb_data = 0
  cont%open    = .true.

end subroutine

!!---------- getter ---------- !!

!> \brief get status of a container (open/close)
!> return .true. if open, .false. if close
function get_status(cont)
  implicit none
  type(CONTAINER), intent(in) :: cont       !< [in] container
  logical                     :: get_status !< [return] is container open

  get_status = cont%open

end function

!> \brief get the number of data in a container
function get_nb_data(cont)
  implicit none
  type(CONTAINER), intent(in) :: cont        !< [in] container
  integer(kind=4)             :: get_nb_data !< [return] number of data

  get_nb_data = cont%nb_data

end function

!> \brief get an element of a container
!> If the container is open, get last data added (and i_data is ignored).
!> If the container is close, get data of index i_data in the data array.
!> \todo : look for data by rank
function get_data(cont, i_data)
  implicit none
  type(CONTAINER), intent(in)           :: cont     !< [in] container
  integer(kind=4), optional, intent(in) :: i_data   !< [in] desired index of data
  type(LIST_DATA)                       :: get_data !< [return]  get a copy of data

  if( cont%open ) then
    if( present(i_data) ) write(*,*) 'WARNING[get_data] : i_data parameter ignored'
    get_data = cont%root%data
  else
    if( .not. present(i_data) ) then
      write(*,*) 'ERROR[get_data] : i_data parameter missing (mandatory when getting data of a close container)'
      stop 1
    end if
    if( i_data < 1 .or. i_data > cont%nb_data ) then
      write(*,*) 'ERROR[get_data] : i_data index out of range : 1 ', i_data, cont%nb_data
      stop 1
    end if
    get_data = cont%data_array(i_data)
  end if

end function

!!---------- search ---------- !!

!> \brief reset internal parameters to run through the linked list
subroutine reset_search(cont)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container

  ! todo : if close error message or open with log message
  if( cont%open ) then
    cont%current_search => cont%root
    cont%prev_search    => cont%root
  end if

end subroutine

!> \brief get the data object of the current object of the linked list
function get_current_search_data(cont)
  implicit none
  type(CONTAINER), intent(in) :: cont                    !< [in] container
  type(LIST_DATA), pointer    :: get_current_search_data !< [return] current data

  get_current_search_data => null()

  if( cont%open ) then
    if( associated(cont%current_search) ) then
      get_current_search_data => cont%current_search%data
    end if
  end if

end function

!> \brief delete current search element from the linked list
subroutine delete_current_list_element(cont)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container

  if( cont%open ) then
    if( associated(cont%current_search,cont%root) ) then
      call list_destroy_element(cont%root,cont%current_search)
      cont%nb_data = cont%nb_data - 1
      cont%current_search => cont%root
      cont%prev_search    => cont%root
    else
      call list_destroy_element(cont%prev_search,cont%current_search)
      cont%nb_data = cont%nb_data - 1
      cont%current_search => cont%prev_search%next
    end if
  end if

end subroutine

!> \brief going to next element in the linked list
subroutine next_search(cont)
  implicit none
  type(CONTAINER), intent(inout) :: cont !< [in,out] container

  if( cont%open ) then
    cont%prev_search    => cont%current_search
    cont%current_search => cont%current_search%next
  end if

end subroutine

!!---------- display ---------- !!

!> \brief display container
subroutine display_container(cont, ifich)
  implicit none
  type(CONTAINER), intent(in) :: cont  !< [in] container to display
  integer(kind=4), optional   :: ifich !< [in] (optional) unit number in which to write
  !
  integer(kind=4) :: i_data, i_unit

  if( cont%nb_data == 0 ) return

  if( present(ifich) ) then
    i_unit = ifich
  else
    i_unit = 6
  end if

  !write(i_unit,*) 'is container open : ', cont%open
  !write(i_unit,*) 'number of data    : ', cont%nb_data
  !write(i_unit,*) 'size of the list  : ', list_count(cont%root)
  !write(i_unit,*) 'is array allocated: ', allocated(cont%data_array)
  !write(i_unit,*) 'size of the array : ', size(cont%data_array)

  if( cont%open ) then
    call list_display(cont%root)
  else
    do i_data = 1, cont%nb_data
      call display_data(cont%data_array(i_data), ifich)
    end do
  end if

end subroutine

