program test_rough_detections

  use anonymous, only : T_object, &
                        get_i4_vector

  use anonymous_ptr_container, only : CONTAINER        => PTR_CONTAINER      , &
                                      erase_container  => erase_ptr_container, &
                                      close_container  => close_ptr_container, &
                                      open_container   => open_ptr_container , &
                                      reset_search                           , &
                                      next_search                            , &
                                      get_current_search_data                , &
                                      delete_current_list_element            , &
                                      display_object_container => display_ptr_container

  use rough_detections

  implicit none

  type(CONTAINER)   :: rough_list ! the container
  type(T_object), pointer :: current

  real(kind=8), dimension(2,6) :: positions
  real(kind=8), dimension(6)   :: radii

  real(kind=8) :: alert,xperiode
  logical      :: isXperiodic
  integer(kind=4), dimension(:), pointer :: i4

  alert = 0.01

  isXperiodic =.false.
  xperiode    = 0.d0

  positions(1:2,1) = (/0.,  0./)
  positions(1:2,2) = (/1.,  0./)
  positions(1:2,3) = (/0.,  1./)
  positions(1:2,4) = (/1.,  1./)
  positions(1:2,5) = (/0.5, 0.5/)
  positions(1:2,6) = (/0.5,-0.5/)

  radii(1:6) = 0.25

  print *,'boxes method'
  call boxes_method(positions, radii, alert, rough_list,isXperiodic, xperiode)

  print *,'display rough container'
  call display_object_container(rough_list)

  print *, 'removing 4-* and *-6 rough'
  call open_container(rough_list)
  call reset_search(rough_list)
  current => get_current_search_data(rough_list)
  do while( associated(current) )
    i4 => get_i4_vector(current)
    if( i4(1) == 4 .or. i4(2) == 6 ) then
      call delete_current_list_element(rough_list)
    else
      call next_search(rough_list)
    end if
    current => get_current_search_data(rough_list)
  end do

  print *,'closing container'
  call close_container(rough_list)

  print *,'display container'
  call display_object_container(rough_list)

  print *,'erase rough container'
  call erase_container(rough_list)

  print *,'boxes method (lists)'
  call boxes_method_lists(positions, radii, alert, rough_list)

  print *,'display rough container'
  call display_object_container(rough_list)

  print *,'erase rough container'
  call erase_container(rough_list)

  print *,'boxes method (lists + sparse matrix)'
  call boxes_method_sparse(positions, radii, alert, rough_list)

  print *,'display rough container'
  call display_object_container(rough_list)

  print *,'erase rough container'
  call erase_container(rough_list)

  print *,'clean module'
  call clean_module()

  print *, 'finished'

end program
