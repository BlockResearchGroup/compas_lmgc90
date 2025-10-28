program test_avatar

  use avatar_container, only : T_avatar_container => PTR_CONTAINER, &
                               erase_ptr_container , &
                               close_ptr_container , &
                               open_ptr_container  , &
                               add_avatar_to_container, &
                               add_bulk_to_container, &
                               add_node_to_container, &
                               add_tact_to_container, &
                               display_avatar_container
 
  implicit none

  type(T_avatar_container) :: avatars ! the avatar container

  integer(kind=4)  :: i
  character(len=5), dimension(:), pointer :: c5
  integer(kind=4),  dimension(:), pointer :: i4
  real(kind=8),     dimension(:), pointer :: r8

  ! 1st avatar
  call add_avatar_to_container(avatars, 1, 'RBDY2', 'MECAx')

  ! with one bulk of a rigid_2d
  allocate(c5(3))
  c5(1:3) = (/'RBDY2', 'PLAIN', 'TMOUx'/)
  allocate(r8(2))
  r8(1:2) = (/0.6D0, 0.2D0/)
  call add_bulk_to_container(avatars, 1, c5, i4, r8)

  ! with one node
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/0.D0, 0.D0/)
  call add_node_to_container(avatars, 1, c5, r8)

  ! with 2 contactors
  allocate(c5(2))
  c5(1:2) = (/'DISKb','redxx'/)
  allocate(r8(3))
  r8(1:3) = (/0.5D0, -0.25D0, 0.D0/)
  call add_tact_to_container(avatars, 1, c5, i4, r8)
  allocate(c5(2))
  c5(1:2) = (/'DISKb','redxx'/)
  allocate(r8(3))
  r8(1:3) = (/0.5D0, +0.25D0, 0.D0/)
  call add_tact_to_container(avatars, 2, c5, i4, r8)

  ! 2nd avatar
  call add_avatar_to_container(avatars, 2, 'MAILx', 'THERx')

  ! with 2 bulk of a TE4xx
  nullify(r8)
  allocate(c5(3))
  c5(1:3) = (/'TE4xx', 'M2DNL', 'Steel'/)
  allocate(i4(4))
  i4(1:4) = (/1, 2, 4, 3/)
  call add_bulk_to_container(avatars, 1, c5, i4, r8)
  allocate(c5(3))
  c5(1:3) = (/'TE4xx', 'M2DNL', 'Steel'/)
  allocate(i4(4))
  i4(1:4) = (/3, 4, 6, 5/)
  call add_bulk_to_container(avatars, 2, c5, i4, r8)

  ! with 6 nodes
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/0.D0, 0.D0/)
  call add_node_to_container(avatars, 1, c5, r8)
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/1.D0, 0.D0/)
  call add_node_to_container(avatars, 2, c5, r8)
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/0.D0, 1.D0/)
  call add_node_to_container(avatars, 3, c5, r8)
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/1.D0, 1.D0/)
  call add_node_to_container(avatars, 4, c5, r8)
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/0.D0, 2.D0/)
  call add_node_to_container(avatars, 5, c5, r8)
  allocate(c5(1))
  c5(1) = 'NO2xx'
  allocate(r8(2))
  r8(1:2) = (/1.D0, 2.D0/)
  call add_node_to_container(avatars, 6, c5, r8)

  ! no tact

  print *, 'display container'
  call display_avatar_container(avatars)

  print *, 'close container'
  call close_ptr_container(avatars)

  print *, 'display container'
  call display_avatar_container(avatars)

  print *, 'erasing container'
  call erase_ptr_container(avatars)

  print *, 'finished'

end program
