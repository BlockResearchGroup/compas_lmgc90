program test_bulk_visit

  use visitor, only : set_visitor

  use anonymous, only : T_object     , &
                        new_object   , &
                        set_rank     , &
                        set_c5_vector, &
                        set_i4_vector, &
                        set_r8_vector

  use anonymous_ptr_container, only : T_object_container => PTR_CONTAINER, &
                                      add_to_ptr_container, &
                                      erase_ptr_container , &
                                      close_ptr_container , &
                                      open_ptr_container  , &
                                      display_ptr_container
 
  use bulk, only : visit_bulk_container

  implicit none

  type(T_object_container) :: bulks ! the bulk container

  integer(kind=4) :: i
  type(T_object), pointer :: blmty
  character(len=128), dimension(:), pointer :: cx

  call set_visitor('write', 6)

  blmty => new_object()
  call set_rank(blmty, 1)
  call set_c5_vector(blmty, (/'RBDY2', 'PLAIN', 'TMOUx'/))
  call set_r8_vector(blmty, (/0.6D0, 0.2D0/))
  call add_to_ptr_container(bulks, blmty)

  blmty => new_object()
  call set_rank(blmty, 2)
  call set_c5_vector(blmty, (/'RBDY3', 'PLAIN', 'TDURx'/))
  call set_r8_vector(blmty, (/0.6D0, 0.2D0, 0.1D0, 0.2D0, 0.3D0, 1.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 1.D0/))
  call add_to_ptr_container(bulks, blmty)

  blmty => new_object()
  call set_rank(blmty, 3)
  call set_c5_vector(blmty, (/'TE4xx', 'M2DNL', 'Steel'/))
  call set_i4_vector(blmty, (/1, 2, 3, 4/))
  call add_to_ptr_container(bulks, blmty)

  print *, 'visit open bulk container'
  call visit_bulk_container(bulks)

  print *, 'close container'
  call close_ptr_container(bulks)

  print *, 'visit close bulk container'
  call visit_bulk_container(bulks)

  print *, 'erasing container'
  call erase_ptr_container(bulks)

  print *, 'finished'

end program
