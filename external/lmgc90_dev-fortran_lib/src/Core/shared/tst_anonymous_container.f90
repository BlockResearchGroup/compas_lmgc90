program test_anonymous_container

  use anonymous_container, only : CONTAINER       , &
                                  erase_container , &
                                  close_container , &
                                  open_container  , &
                                  add_object_to_container, &
                                  display_object_container

  implicit none

  type(CONTAINER) :: objects ! the container

  integer(kind=4) :: i, j
  character(len=5),   dimension(:), pointer :: c5
  integer(kind=4),    dimension(:), pointer :: i4
  real(kind=8),       dimension(:), pointer :: r8
  character(len=128), dimension(:), pointer :: cx

  c5 => null()
  i4 => null()
  r8 => null()
  cx => null()

  allocate(c5(1)); c5(1)   = 'blabl'
  allocate(r8(3)); r8(1:3) = (/3.3D0, 2.2D0, 1.1D0/)
  allocate(cx(1)); cx(1)   = 'mieux'
  call add_object_to_container(objects, 1, c5, i4, r8, cx)

  allocate(c5(1)); c5(1)   = 'blebl'
  allocate(i4(1)); i4(1)   = 1
  allocate(r8(2)); r8(1:2) = (/2.2D0, 1.1D0/)
  allocate(cx(3))
  call add_object_to_container(objects, 2, c5, i4, r8, cx)
  cx(1) = 'ça'; cx(2) = 'marche'; cx(3) = '?'
  ! la reponse est oui

  allocate(c5(1)); c5(1)   = 'blibl'
  allocate(i4(2)); i4(1:2) = (/1, 2/)
  allocate(r8(1)); r8(1)   = 1.1D0
  nullify(cx)
  call add_object_to_container(objects, 3, c5, i4, r8, cx)

  allocate(c5(1)); c5(1)   = 'blobl'
  allocate(i4(3)); i4(1:3) = (/1, 2, 3/)
  nullify(r8)
  allocate(cx(2)); cx(1)   = 'pas'; cx(2) = 'mieux'
  call add_object_to_container(objects, 4, c5, i4, r8, cx)

  print *, 'display content of container'
  call display_object_container(objects)

  print *, 'close container'
  call close_container(objects)

  print *, 'display content of container'
  call display_object_container(objects)

  print *, 'open container'
  call open_container(objects)

  print *, 'display content of container'
  call display_object_container(objects)

  print *, 'erasing container'
  call erase_container(objects)

  print *, 'display content of container'
  call display_object_container(objects)

   ! consider empty objects
  c5 => null()
  i4 => null()
  r8 => null()
  cx => null()

  print *, 'dummy load tests:'

  ! test opened containers
  print *, '   * for opened containers'
  ! repeat fifty times
  do i=1, 50
     ! fill a container with dummy data
     do j=1, 20000
        call add_object_to_container(objects, j, c5, i4, r8, cx)
     end do
     ! delete it
     call erase_container(objects)
  end do

  ! test for closed containers
  print *, '   * for closed containers'
  ! repeat fifty times
  do i=1, 50
     ! fill a container with dummy data
     do j=1, 20000
        call add_object_to_container(objects, j, c5, i4, r8, cx)
     end do
     ! close it
     call close_container(objects)
     ! delete it
     call erase_container(objects)
  end do

  print *, 'finished'

end program
