! linkedlist.f90 --
!     Include file for defining linked lists where each element holds
!     the same kind of data
!
!     Note:
!     You should only use pointer variables of this type, no
!     ordinary variables, as sometimes the memory pointed to
!     will be deallocated. The subroutines and functions
!     are designed to minimize mistakes (for instance: using
!     = instead of =>)
!
!     $Id: linkedlist.f90,v 1.3 2007/01/26 09:56:43 arjenmarkus Exp $
!
!     rm: to use the list_data type must provide the
!         erase_data and dispaly_data subroutines
!
!     rm: the type has been modifield from the flib to manipulate
!         object with the pointer attribute instead of an object
!
! Define the linked-list data type
!
type, public :: LINKED_LIST
    private
    type(LINKED_LIST), pointer :: next => null()
    type(LIST_DATA),   pointer :: data => null()
end type LINKED_LIST

! to work on a list
!public list_create        , &
!       list_destroy       , &
!       list_count         , &
!       list_next          , &
!       list_insert        , &
!       list_insert_head   , &
!       list_delete_element, &
!       list_get_data      , &
!       list_put_data      , &
!       list_display ! <- rm: added
 
