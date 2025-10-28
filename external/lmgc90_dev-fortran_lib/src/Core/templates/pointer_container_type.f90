! Defines a type of container which use
! shallow copy with pointer objects within its array
! Thus the list_data type must provide
! the subroutines : erase_data and display_data
! for the linked list and open_data, close_data
! for the container

include 'linkedlist_type.f90'

type, public :: PTR_LIST_DATA
  type(LIST_DATA), pointer :: data
end type

type, public :: PTR_CONTAINER
  private
  integer(kind=4) :: nb_data = 0
  logical         :: open    = .true.

  type(LINKED_LIST), pointer :: root => null()
  type(PTR_LIST_DATA), dimension(:), allocatable :: data_array

  type(LINKED_LIST), pointer :: current_search => null()
  type(LINKED_LIST), pointer :: prev_search    => null()
end type

! to work on a container
public add_to_ptr_container, &
       close_ptr_container , &
       open_ptr_container  , &
       erase_ptr_container , &
       get_status      , &
       get_nb_data     , &
       get_data        , &
       reset_search    , &
       next_search     , &
       get_current_search_data    , &
       delete_current_list_element, &
       display_ptr_container
